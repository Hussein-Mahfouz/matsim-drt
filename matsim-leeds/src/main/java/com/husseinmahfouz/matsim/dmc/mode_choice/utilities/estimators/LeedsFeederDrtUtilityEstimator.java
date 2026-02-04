package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.LinkedList;
import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.UtilityEstimator;
import org.eqasim.core.simulation.modes.drt.mode_choice.variables.DrtVariables;
import org.eqasim.core.simulation.modes.feeder_drt.router.FeederDrtRoutingModule.FeederDrtTripSegmentType;
import org.matsim.api.core.v01.population.Activity;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsDrtPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPtPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPtVariables;

/**
 * Feeder DRT utility estimator that delegates to PT and DRT estimators, then applies
 * corrections to ensure components are counted correctly for composite trips.
 * 
 * ====================================================================================
 * UTILITY EQUATION
 * ====================================================================================
 * 
 * U_feeder = ASC_pt                                            (single constant, Bus or Rail)
 *          + β_inc>50k * 1(Income > 50k)                       (single income effect, if bus-dominant)
 *          + Σ [PT segment time utilities]                     (additive across segments)
 *          + Σ [DRT segment time utilities]                    (additive across segments)
 *          + β_cost * ln(Cost_PT + Cost_DRT)                   (log of TOTAL cost)
 * 
 * Where PT segment utility includes:
 *   (β_IVT + β_peak * 1(Peak)) * f(IVT, λ_time) + β_OVT * f(OVT, λ_OVT)
 * 
 * And DRT segment utility includes:
 *   (β_IVT_drt + β_peak * 1(Peak)) * f(IVT, λ_time) + β_OVT_drt * f(OVT, λ_OVT)
 * 
 * ====================================================================================
 * HOW IT WORKS
 * ====================================================================================
 * 
 * 1. SEGMENT ITERATION:
 *    The trip is split into segments (PT legs and DRT legs) using the same logic as
 *    DefaultFeederDrtUtilityEstimator. Each segment is identified by "interaction"
 *    activities that mark boundaries between PT and DRT legs.
 * 
 * 2. DELEGATION:
 *    Each segment's utility is calculated by delegating to:
 *    - LeedsPtUtilityEstimator for PT segments (bus/rail legs)
 *    - LeedsDrtUtilityEstimator for DRT segments
 *    
 *    This ensures travel time utilities are calculated correctly with all transformations.
 * 
 * 3. REDUNDANCY CORRECTION:
 *    The delegated estimators each add their own ASC and person-specific effects.
 *    For a composite trip, we want these counted only ONCE. The correction:
 *    - Tracks which ASC (Bus or Rail) was added by each PT segment
 *    - Keeps exactly 1 PT ASC (the first one encountered)
 *    - Subtracts ALL DRT ASCs
 *    - Subtracts extra income effects (if multiple bus-dominant PT segments)
 * 
 * 4. COST CORRECTION:
 *    Because cost uses log transformation, we cannot simply sum utilities:
 *      β*log(cost_PT) + β*log(cost_DRT) ≠ β*log(cost_PT + cost_DRT)
 *    
 *    The correction:
 *    - Tracks raw costs from each segment via predictors
 *    - Subtracts the "wrong" sum-of-logs that was added by delegated estimators
 *    - Adds the "correct" log-of-sum for total trip cost
 * 
 * ====================================================================================
 * IMPORTANT NOTES
 * ====================================================================================
 * 
 * - Peak effects are INSIDE the time utility (multiplied by transformed time).
 *   Since Box-Cox transformed times are additive across segments, no peak correction
 *   is needed here.
 * 
 * - The feeder mode uses the ASC of the FIRST PT segment (Bus or Rail, depending on
 *   which mode dominates that segment).
 * 
 * - Income effects are only applied for bus-dominant PT segments (following the
 *   original Tsoleridis specification where income effect is bus-specific).
 */
public class LeedsFeederDrtUtilityEstimator implements UtilityEstimator {

    private final LeedsModeParameters parameters;
    private final LeedsPtUtilityEstimator ptEstimator;
    private final LeedsDrtUtilityEstimator drtEstimator;
    
    private final LeedsPtPredictor ptPredictor;
    private final LeedsDrtPredictor drtPredictor;
    private final LeedsPersonPredictor personPredictor;

    @Inject
    public LeedsFeederDrtUtilityEstimator(
            LeedsModeParameters parameters,
            LeedsPtUtilityEstimator ptEstimator,
            LeedsDrtUtilityEstimator drtEstimator,
            LeedsPtPredictor ptPredictor,
            LeedsDrtPredictor drtPredictor,
            LeedsPersonPredictor personPredictor) {
        this.parameters = parameters;
        this.ptEstimator = ptEstimator;
        this.drtEstimator = drtEstimator;
        this.ptPredictor = ptPredictor;
        this.drtPredictor = drtPredictor;
        this.personPredictor = personPredictor;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip, 
            List<? extends PlanElement> elements) {
        
        double totalUtility = 0.0;
        
        // Counters for redundancy correction
        int drtSegmentCount = 0;
        int busAscCount = 0;      // Track how many times Bus ASC was added
        int railAscCount = 0;     // Track how many times Rail ASC was added
        int busIncomeCount = 0;   // Track how many times income effect was added (bus-dominant segments)
        
        // Accumulators for cost correction
        double totalPtCost_MU = 0.0;
        double totalDrtCost_MU = 0.0;

        // ========================================================================
        // SEGMENT ITERATION
        // ========================================================================
        List<PlanElement> currentSegment = new LinkedList<>();
        String stageActivityType = null;
        FeederDrtTripSegmentType previousSegmentType = null;

        for (PlanElement element : elements) {
            if (element instanceof Activity) {
                Activity stageActivity = (Activity) element;
                
                // Check if this is an interaction activity marking segment boundary
                if (stageActivityType != null && stageActivity.getType().equals(stageActivityType)) {
                    
                    if (previousSegmentType == null) {
                        throw new IllegalStateException(
                            "Encountered Feeder interaction activity before any leg");
                    }

                    // Process the completed segment
                    if (previousSegmentType.equals(FeederDrtTripSegmentType.MAIN)) {
                        // PT segment
                        totalUtility += ptEstimator.estimateUtility(person, trip, currentSegment);
                        
                        // Track which ASC was added and whether income effect applied
                        LeedsPtVariables ptVars = ptPredictor.predictVariables(person, trip, currentSegment);
                        if (ptVars.railTravelTime_min > ptVars.busTravelTime_min) {
                            railAscCount++;
                        } else {
                            busAscCount++;
                            // Income effect only applies to bus-dominant segments
                            LeedsPersonVariables personVars = personPredictor.predictVariables(person, trip, elements);
                            if (personVars.indIncomeSPC > 50000) {
                                busIncomeCount++;
                            }
                        }
                        totalPtCost_MU += ptVars.cost_MU;
                        
                    } else if (previousSegmentType.equals(FeederDrtTripSegmentType.DRT)) {
                        // DRT segment
                        totalUtility += drtEstimator.estimateUtility(person, trip, currentSegment);
                        drtSegmentCount++;
                        
                        DrtVariables drtVars = drtPredictor.predictVariables(person, trip, currentSegment);
                        totalDrtCost_MU += drtVars.cost_MU;
                    }

                    currentSegment.clear();
                    continue;
                }
            }

            // Add element to current segment
            currentSegment.add(element);

            // Detect segment type from leg attributes
            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                
                // Set stage activity type on first leg
                if (stageActivityType == null) {
                    String routingMode = leg.getRoutingMode();
                    stageActivityType = routingMode + " interaction";
                }

                // Get segment type from leg attribute
                Object segmentTypeAttr = leg.getAttributes().getAttribute("currentSegmentType");
                if (segmentTypeAttr != null) {
                    if (segmentTypeAttr instanceof FeederDrtTripSegmentType) {
                        previousSegmentType = (FeederDrtTripSegmentType) segmentTypeAttr;
                    } else {
                        previousSegmentType = FeederDrtTripSegmentType.valueOf(segmentTypeAttr.toString());
                    }
                }
            }
        }

        // Process final segment
        if (!currentSegment.isEmpty() && previousSegmentType != null) {
            if (previousSegmentType.equals(FeederDrtTripSegmentType.MAIN)) {
                totalUtility += ptEstimator.estimateUtility(person, trip, currentSegment);
                
                LeedsPtVariables ptVars = ptPredictor.predictVariables(person, trip, currentSegment);
                if (ptVars.railTravelTime_min > ptVars.busTravelTime_min) {
                    railAscCount++;
                } else {
                    busAscCount++;
                    LeedsPersonVariables personVars = personPredictor.predictVariables(person, trip, elements);
                    if (personVars.indIncomeSPC > 50000) {
                        busIncomeCount++;
                    }
                }
                totalPtCost_MU += ptVars.cost_MU;
                
            } else if (previousSegmentType.equals(FeederDrtTripSegmentType.DRT)) {
                totalUtility += drtEstimator.estimateUtility(person, trip, currentSegment);
                drtSegmentCount++;
                
                DrtVariables drtVars = drtPredictor.predictVariables(person, trip, currentSegment);
                totalDrtCost_MU += drtVars.cost_MU;
            }
        }

        // ========================================================================
        // CORRECTIONS
        // ========================================================================
        
        // Correction 1: Remove redundant ASCs and person-specific effects
        totalUtility -= calculateRedundantUtility(busAscCount, railAscCount, drtSegmentCount, busIncomeCount);
        
        // Correction 2: Fix cost utility (sum-of-logs → log-of-sum)
        totalUtility += calculateCostCorrection(totalPtCost_MU, totalDrtCost_MU);

        return totalUtility;
    }

    /**
     * Corrects cost utility from sum-of-logs to log-of-sum.
     */
    private double calculateCostCorrection(double ptCost, double drtCost) {
        double betaCost = parameters.betaCost_u_MU;
        double correction = 0.0;

        double totalCost = ptCost + drtCost;
        if (totalCost > 0) {
            correction += betaCost * Math.log(totalCost);
        }

        if (ptCost > 0) {
            correction -= betaCost * Math.log(ptCost);
        }
        if (drtCost > 0) {
            correction -= betaCost * Math.log(drtCost);
        }

        return correction;
    }

    /**
     * Calculates utility components that were double-counted and need to be subtracted.
     * 
     * Strategy:
     * - Keep exactly 1 PT ASC (first encountered: prioritize Bus if any, else Rail)
     * - Subtract ALL DRT ASCs
     * - Subtract extra income effects (keep at most 1)
     * 
     * @param busAscCount Number of bus-dominant PT segments (each added Bus ASC)
     * @param railAscCount Number of rail-dominant PT segments (each added Rail ASC)
     * @param drtSegmentCount Number of DRT segments (each added DRT ASC)
     * @param busIncomeCount Number of times income effect was added (bus-dominant high-income)
     */
    private double calculateRedundantUtility(int busAscCount, int railAscCount, 
            int drtSegmentCount, int busIncomeCount) {
        
        double redundant = 0.0;
        
        // =====================================================================
        // ALTERNATIVE SPECIFIC CONSTANTS (ASCs)
        // =====================================================================
        // We want exactly 1 PT ASC for the whole feeder trip.
        // Strategy: Keep the first Bus ASC if any; otherwise keep the first Rail ASC.
        // =====================================================================
        
        if (busAscCount > 0) {
            // Keep 1 Bus ASC, subtract extras
            redundant += (busAscCount - 1) * parameters.leedsPT.alpha_u_Bus;
            // Subtract ALL Rail ASCs (Bus ASC is the "main" one)
            redundant += railAscCount * parameters.leedsPT.alpha_u_Rail;
        } else if (railAscCount > 0) {
            // No Bus segments, keep 1 Rail ASC, subtract extras
            redundant += (railAscCount - 1) * parameters.leedsPT.alpha_u_Rail;
        }
        
        // Subtract ALL DRT ASCs
        redundant += drtSegmentCount * parameters.drt.alpha_u;
        
        // =====================================================================
        // PERSON-SPECIFIC EFFECTS (Income > 50k)
        // =====================================================================
        // Income effect only applies to bus-dominant segments.
        // Keep at most 1 income effect.
        // =====================================================================
        
        if (busIncomeCount > 1) {
            redundant += (busIncomeCount - 1) * parameters.leedsPT.betaIncome50k;
        }
        
        return redundant;
    }
}