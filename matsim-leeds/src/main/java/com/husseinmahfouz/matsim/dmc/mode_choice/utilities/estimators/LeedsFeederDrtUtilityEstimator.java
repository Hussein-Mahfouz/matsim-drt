package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.LinkedList;
import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.UtilityEstimator;
import org.eqasim.core.simulation.modes.feeder_drt.router.FeederDrtRoutingModule.FeederDrtTripSegmentType;
import org.matsim.api.core.v01.population.Activity;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
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
 *   - Cost utilities (these SHOULD be summed - no correction needed)
 * 
 * For a feeder trip with 1 PT segment + 1 DRT segment, we get:
 *   - 1x PT constant + 1x DRT constant  → We want only 1 constant total
 *   - 1x PT income effect               → We want only 1 income effect total
 *   - 1x PT peak effect                 → We want only 1 peak effect total
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
 * WHAT DOES NOT NEED CORRECTION:
 * ============================================================================
 * 
 * - Travel time utilities: These SHOULD sum across segments (time on DRT + time on PT)
 * - Waiting time utilities: These SHOULD sum across segments
 * - Access/egress time utilities: These SHOULD sum across segments
 * - Monetary cost utilities: See note below about cost calculation
 * 
 */
public class LeedsFeederDrtUtilityEstimator implements UtilityEstimator {

    private final LeedsModeParameters parameters;
    private final LeedsPtUtilityEstimator ptEstimator;
    private final LeedsDrtUtilityEstimator drtEstimator;
    private final LeedsPersonPredictor personPredictor;
    private final LeedsSpatialPredictor spatialPredictor;

    @Inject
    public LeedsFeederDrtUtilityEstimator(
            LeedsModeParameters parameters,
            LeedsPtUtilityEstimator ptEstimator,
            LeedsDrtUtilityEstimator drtEstimator,
            LeedsPersonPredictor personPredictor,
            LeedsSpatialPredictor spatialPredictor) {
        this.parameters = parameters;
        this.ptEstimator = ptEstimator;
        this.drtEstimator = drtEstimator;
        this.personPredictor = personPredictor;
        this.spatialPredictor = spatialPredictor;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip, List<? extends PlanElement> elements) {
        
        double totalUtility = 0.0;
        int ptSegmentCount = 0;
        int drtSegmentCount = 0;

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

                    // DELEGATE to existing estimators
                    if (previousSegmentType.equals(FeederDrtTripSegmentType.MAIN)) {
                        totalUtility += ptEstimator.estimateUtility(person, trip, currentSegment);
                        ptSegmentCount++;
                    } else if (previousSegmentType.equals(FeederDrtTripSegmentType.DRT)) {
                        totalUtility += drtEstimator.estimateUtility(person, trip, currentSegment);
                        drtSegmentCount++;
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
            } else if (previousSegmentType.equals(FeederDrtTripSegmentType.DRT)) {
                totalUtility += drtEstimator.estimateUtility(person, trip, currentSegment);
                drtSegmentCount++;
            }
        }

        // --- CORRECTION: Subtract redundant components ---
        totalUtility -= calculateRedundantUtility(person, trip, elements, ptSegmentCount, drtSegmentCount);

        return totalUtility;
    }

    /**
     * Calculate the utility that was double-counted and needs to be subtracted.
     * 
     * Components that should only be counted ONCE per trip:
     * - Alternative Specific Constants (ASCs)
     * - Person-specific effects (income)
     * - Time-of-day effects (AM/PM peak)
     * 
     * Components that should NOT be corrected (they correctly sum):
     * - Travel time utilities
     * - Waiting time utilities  
     * - Monetary cost utilities (calculated per-segment, summing is correct)
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
        // 
        // For feeder trips, we assume bus is the main PT mode.
        // If you have rail-based feeder, you may need more complex logic.
        // ---------------------------------------------------------------------
        if (ptSegmentCount > 1 && personVars.indIncomeSPC > 50000) {
            redundant += (ptSegmentCount - 1) * parameters.leedsPT.betaIncome50k;
        }
        
        // ---------------------------------------------------------------------
        // EXTENDING FOR OTHER PERSON EFFECTS:
        // 
        // Example: If you add betaMale to PT estimator:
        //   if (ptSegmentCount > 1 && personVars.isMale) {
        //       redundant += (ptSegmentCount - 1) * parameters.leedsPT.betaMale;
        //   }
        // 
        // Example: If you add betaAge to DRT estimator:
        //   if (drtSegmentCount > 1 && personVars.age < 30) {
        //       redundant += (drtSegmentCount - 1) * parameters.drt.betaYoung;
        //   }
        // 
        // Example: If betaStudent is in BOTH PT and DRT:
        //   int segmentsWithStudent = 0;
        //   if (personVars.isStudent) {
        //       segmentsWithStudent = ptSegmentCount + drtSegmentCount;
        //   }
        //   if (segmentsWithStudent > 1) {
        //       redundant += (segmentsWithStudent - 1) * parameters.common.betaStudent;
        //   }
        // ---------------------------------------------------------------------
        
        // =====================================================================
        // TIME-OF-DAY EFFECTS
        // =====================================================================
        
        // ---------------------------------------------------------------------
        // AM/PM Peak effect (currently only in LeedsPtUtilityEstimator)
        // Applied when: isAMPeak OR isPMPeak
        // ---------------------------------------------------------------------
        if (ptSegmentCount > 1 && (spatialVars.isAMPeak || spatialVars.isPMPeak)) {
            redundant += (ptSegmentCount - 1) * parameters.leedsPT.betaAmPmPeakBus;
        }
        
        // ---------------------------------------------------------------------
        // EXTENDING FOR OTHER TIME EFFECTS:
        // 
        // Example: If you add night penalty to DRT:
        //   if (drtSegmentCount > 1 && spatialVars.isNight) {
        //       redundant += (drtSegmentCount - 1) * parameters.drt.betaNight;
        //   }
        // ---------------------------------------------------------------------
        
        // =====================================================================
        // DRT REJECTION PENALTY - NO CORRECTION NEEDED
        // =====================================================================
        // See explanation in class comments: A feeder trip has at most 1 DRT segment
        // that could be rejected (access DRT or egress DRT, not both in same trip).
        // The DRT estimator correctly applies the penalty once per DRT segment.
        // =====================================================================
        
        return redundant;
    }
}