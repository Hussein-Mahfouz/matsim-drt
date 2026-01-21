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
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;

/**
 * Feeder DRT utility estimator that delegates to existing PT and DRT estimators,
 * then corrects for double-counted components.
 * 
 * ============================================================================
 * HOW THE REDUNDANCY CORRECTION WORKS:
 * ============================================================================
 * 
 * When we delegate to LeedsPtUtilityEstimator and LeedsDrtUtilityEstimator,
 * each call to estimateUtility() adds:
 *   - A mode constant (ASC)
 *   - Person-specific effects (income, age, etc.)
 *   - Time-of-day effects (AM/PM peak)
 *   - Travel time utilities (these SHOULD be summed - no correction needed)
 *   - Cost utilities (these need correction due to log transformation)
 * 
 * For a feeder trip with 1 PT segment + 1 DRT segment, we get:
 *   - 1x PT constant + 1x DRT constant  → We want only 1 constant total
 *   - 1x PT income effect               → We want only 1 income effect total
 *   - 1x PT peak effect                 → We want only 1 peak effect total
 *   - β*log(PT_cost) + β*log(DRT_cost)  → We want β*log(PT_cost + DRT_cost)
 * 
 * The correction subtracts the "extras" so only one instance remains.
 * 
 * ============================================================================
 * HOW TO EXTEND FOR NEW PARAMETERS:
 * ============================================================================
 * 
 * If you add a new person-specific or time-of-day parameter to an estimator,
 * you need to add corresponding correction logic here. Follow this pattern:
 * 
 * 1. Identify WHERE the parameter is applied:
 *    - In LeedsPtUtilityEstimator only? → Correct based on ptSegmentCount
 *    - In LeedsDrtUtilityEstimator only? → Correct based on drtSegmentCount
 *    - In both? → Correct based on totalSegments
 * 
 * 2. Identify the CONDITION for the parameter:
 *    - Is it always applied? (like ASC)
 *    - Is it conditional? (like income > 50k, or isPeak)
 * 
 * 3. Add correction in calculateRedundantUtility():
 *    
 *    // TEMPLATE for PT-only parameter:
 *    if (ptSegmentCount > 1 && <condition>) {
 *        redundant += (ptSegmentCount - 1) * parameters.<parameter_value>;
 *    }
 *    
 *    // TEMPLATE for DRT-only parameter:
 *    if (drtSegmentCount > 1 && <condition>) {
 *        redundant += (drtSegmentCount - 1) * parameters.<parameter_value>;
 *    }
 *    
 *    // TEMPLATE for parameter in BOTH estimators (want exactly 1 total):
 *    int totalWithParameter = 0;
 *    if (<PT applies parameter>) totalWithParameter += ptSegmentCount;
 *    if (<DRT applies parameter>) totalWithParameter += drtSegmentCount;
 *    if (totalWithParameter > 1 && <condition>) {
 *        redundant += (totalWithParameter - 1) * parameters.<parameter_value>;
 *    }
 * 
 * ============================================================================
 * COST CORRECTION:
 * ============================================================================
 * 
 * Because we use log(cost) transformation:
 *   β*log(cost_PT) + β*log(cost_DRT) ≠ β*log(cost_PT + cost_DRT)
 * 
 * We correct by:
 *   1. Tracking the raw costs from each segment via predictors
 *   2. Subtracting the "wrong" delegated cost utilities
 *   3. Adding the "correct" total cost utility
 * 
 * This ensures the feeder trip has a single cost utility based on TOTAL cost.
 * 
 */
public class LeedsFeederDrtUtilityEstimator implements UtilityEstimator {

    private final LeedsModeParameters parameters;
    private final LeedsPtUtilityEstimator ptEstimator;
    private final LeedsDrtUtilityEstimator drtEstimator;
    
    // Predictors needed to access raw costs for correction
    private final LeedsPtPredictor ptPredictor;
    private final LeedsDrtPredictor drtPredictor;
    
    private final LeedsPersonPredictor personPredictor;
    private final LeedsSpatialPredictor spatialPredictor;

    @Inject
    public LeedsFeederDrtUtilityEstimator(
            LeedsModeParameters parameters,
            LeedsPtUtilityEstimator ptEstimator,
            LeedsDrtUtilityEstimator drtEstimator,
            LeedsPtPredictor ptPredictor,
            LeedsDrtPredictor drtPredictor,
            LeedsPersonPredictor personPredictor,
            LeedsSpatialPredictor spatialPredictor) {
        this.parameters = parameters;
        this.ptEstimator = ptEstimator;
        this.drtEstimator = drtEstimator;
        this.ptPredictor = ptPredictor;
        this.drtPredictor = drtPredictor;
        this.personPredictor = personPredictor;
        this.spatialPredictor = spatialPredictor;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip, List<? extends PlanElement> elements) {
        
        double totalUtility = 0.0;
        
        // Counters for redundancy correction
        int ptSegmentCount = 0;
        int drtSegmentCount = 0;
        
        // Accumulators for cost correction
        double totalPtCost_MU = 0.0;
        double totalDrtCost_MU = 0.0;

        // --- Iterate through segments (same logic as DefaultFeederDrtUtilityEstimator) ---
        List<PlanElement> currentSegment = new LinkedList<>();
        String stageActivityType = null;
        FeederDrtTripSegmentType previousSegmentType = null;

        for (PlanElement element : elements) {
            if (element instanceof Activity) {
                Activity stageActivity = (Activity) element;
                if (stageActivityType != null && stageActivity.getType().equals(stageActivityType)) {
                    
                    if (previousSegmentType == null) {
                        throw new IllegalStateException("Encountered Feeder interaction activity before any leg");
                    }

                    // Process completed segment
                    if (previousSegmentType.equals(FeederDrtTripSegmentType.MAIN)) {
                        // 1. Delegate utility calculation to PT estimator
                        totalUtility += ptEstimator.estimateUtility(person, trip, currentSegment);
                        ptSegmentCount++;
                        
                        // 2. Track raw cost for correction (predictors are cached - efficient)
                        LeedsPtVariables ptVars = ptPredictor.predictVariables(person, trip, currentSegment);
                        totalPtCost_MU += ptVars.cost_MU;
                        
                    } else if (previousSegmentType.equals(FeederDrtTripSegmentType.DRT)) {
                        // 1. Delegate utility calculation to DRT estimator
                        totalUtility += drtEstimator.estimateUtility(person, trip, currentSegment);
                        drtSegmentCount++;
                        
                        // 2. Track raw cost for correction
                        DrtVariables drtVars = drtPredictor.predictVariables(person, trip, currentSegment);
                        totalDrtCost_MU += drtVars.cost_MU;
                    }

                    currentSegment.clear();
                    continue;
                }
            }

            currentSegment.add(element);

            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                if (stageActivityType == null) {
                    String routingMode = leg.getRoutingMode();
                    stageActivityType = routingMode + " interaction";
                }

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
                ptSegmentCount++;
                LeedsPtVariables ptVars = ptPredictor.predictVariables(person, trip, currentSegment);
                totalPtCost_MU += ptVars.cost_MU;
            } else if (previousSegmentType.equals(FeederDrtTripSegmentType.DRT)) {
                totalUtility += drtEstimator.estimateUtility(person, trip, currentSegment);
                drtSegmentCount++;
                DrtVariables drtVars = drtPredictor.predictVariables(person, trip, currentSegment);
                totalDrtCost_MU += drtVars.cost_MU;
            }
        }

        // --- CORRECTION 1: ASC and Person/Time effects ---
        totalUtility -= calculateRedundantUtility(person, trip, elements, ptSegmentCount, drtSegmentCount);
        
        // --- CORRECTION 2: Cost (convert sum-of-logs to log-of-sum) ---
        totalUtility += calculateCostCorrection(totalPtCost_MU, totalDrtCost_MU);

        return totalUtility;
    }

    /**
     * Corrects cost utility from sum-of-logs to log-of-sum.
     * 
     * Problem:  Delegated estimators calculate β*log(PT_cost) + β*log(DRT_cost)
     * Solution: We want β*log(PT_cost + DRT_cost)
     * 
     * This method returns: [what we want] - [what we have]
     *                    = β*log(total) - β*log(PT) - β*log(DRT)
     */
    private double calculateCostCorrection(double ptCost, double drtCost) {
        double betaCost = parameters.betaCost_u_MU;
        double correction = 0.0;

        // 1. Add what we WANT: utility of TOTAL cost
        double totalCost = ptCost + drtCost;
        if (totalCost > 0) {
            correction += betaCost * Math.log(totalCost);
        }

        // 2. Subtract what delegated estimators already added
        // (These were added inside ptEstimator.estimateUtility() and drtEstimator.estimateUtility())
        if (ptCost > 0) {
            correction -= betaCost * Math.log(ptCost);
        }
        if (drtCost > 0) {
            correction -= betaCost * Math.log(drtCost);
        }

        return correction;
    }

    /**
     * Calculate the utility that was double-counted and needs to be subtracted.
     * 
     * Components that should only be counted ONCE per trip:
     * - Alternative Specific Constants (ASCs)
     * - Person-specific effects (income)
     * - Time-of-day effects (AM/PM peak)
     */
    private double calculateRedundantUtility(Person person, DiscreteModeChoiceTrip trip, 
            List<? extends PlanElement> elements, int ptSegmentCount, int drtSegmentCount) {
        
        double redundant = 0.0;
        
        // =====================================================================
        // ALTERNATIVE SPECIFIC CONSTANTS (ASCs)
        // =====================================================================
        // Currently: ptSegmentCount * PT_ASC + drtSegmentCount * DRT_ASC
        // Desired:   1 * PT_ASC (we choose PT as the "base" for feeder mode)
        // 
        // Strategy: Keep 1 PT constant, subtract all DRT constants and extra PT constants
        // =====================================================================
        
        // Subtract extra PT constants (keep 1)
        if (ptSegmentCount > 1) {
            redundant += (ptSegmentCount - 1) * parameters.leedsPT.alpha_u_Bus;
        }
        
        // Subtract ALL DRT constants (PT constant serves as the single ASC)
        redundant += drtSegmentCount * parameters.drt.alpha_u;
        
        // =====================================================================
        // PERSON-SPECIFIC EFFECTS
        // =====================================================================
        LeedsPersonVariables personVars = personPredictor.predictVariables(person, trip, elements);
        LeedsSpatialVariables spatialVars = spatialPredictor.predictVariables(person, trip, elements);
        
        // ---------------------------------------------------------------------
        // Income effect (currently only in LeedsPtUtilityEstimator for bus)
        // Applied when: busTravelTime > railTravelTime AND income > 50k
        // ---------------------------------------------------------------------
        if (ptSegmentCount > 1 && personVars.indIncomeSPC > 50000) {
            redundant += (ptSegmentCount - 1) * parameters.leedsPT.betaIncome50k;
        }
        
        // =====================================================================
        // TIME-OF-DAY EFFECTS
        // =====================================================================
        
        // AM/PM Peak effect (currently only in LeedsPtUtilityEstimator)
        if (ptSegmentCount > 1 && (spatialVars.isAMPeak || spatialVars.isPMPeak)) {
            redundant += (ptSegmentCount - 1) * parameters.leedsPT.betaAmPmPeakBus;
        }
        
        return redundant;
    }
}