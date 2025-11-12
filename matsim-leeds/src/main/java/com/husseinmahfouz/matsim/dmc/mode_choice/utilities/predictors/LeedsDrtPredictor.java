package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import org.eqasim.core.simulation.modes.drt.mode_choice.predictors.DrtPredictor;
import org.eqasim.core.simulation.modes.drt.mode_choice.variables.DrtVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import javax.inject.Inject;
import java.util.List;

import com.husseinmahfouz.matsim.drt.waiting.DrtWaitingTimeProvider;

public class LeedsDrtPredictor implements DrtPredictor {
    
    private final DrtPredictor basePredictor;
    private final DrtWaitingTimeProvider waitingTimeProvider;
    
    @Inject
    public LeedsDrtPredictor(DrtPredictor basePredictor, 
                             DrtWaitingTimeProvider waitingTimeProvider) {
        this.basePredictor = basePredictor;
        this.waitingTimeProvider = waitingTimeProvider;
    }
    
    @Override
    public DrtVariables predictVariables(Person person, 
                                        DiscreteModeChoiceTrip trip,
                                        List<? extends PlanElement> elements) {
        
        // Get base prediction (travel time, access/egress, cost)
        DrtVariables baseVars = basePredictor.predictVariables(person, trip, elements);
        
        // Get trip departure time
        double departureTime = trip.getDepartureTime();
        
        // Extract mode from elements (should be a Leg with DRT mode)
        String mode = extractDrtMode(elements);
        
        // Get actual iteration-based waiting time for this mode and time
        double actualWaitingTime_sec = waitingTimeProvider.getWaitingTime(mode, departureTime);
        double actualWaitingTime_min = actualWaitingTime_sec / 60.0;
        
        // Return new variables with updated waiting time
        return new DrtVariables(
            baseVars.travelTime_min,
            baseVars.cost_MU,
            baseVars.euclideanDistance_km,
            actualWaitingTime_min,  // Use actual time-aware waiting time
            baseVars.accessEgressTime_min
        );
    }
    
    /**
     * Extract DRT mode from plan elements
     * @param elements Plan elements (should contain a Leg)
     * @return DRT mode string (e.g., "drtNW", "drtNE")
     */
    private String extractDrtMode(List<? extends PlanElement> elements) {
        for (PlanElement element : elements) {
            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                String mode = leg.getMode();
                if (mode.startsWith("drt")) {  // Matches "drtNW", "drtNE", etc.
                    return mode;
                }
            }
        }
        // Fallback (should not happen)
        return "drt";
    }
}