package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import org.eqasim.core.simulation.modes.drt.mode_choice.predictors.DefaultDrtPredictor;
import org.eqasim.core.simulation.modes.drt.mode_choice.predictors.DrtPredictor;
import org.eqasim.core.simulation.modes.drt.mode_choice.variables.DrtVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import com.google.inject.Inject;
import java.util.List;

import com.husseinmahfouz.matsim.drt.waiting.DrtWaitingTimeProvider;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LeedsDrtPredictor implements DrtPredictor {

    private static final Logger log = LogManager.getLogger(LeedsDrtPredictor.class); 

    public final DefaultDrtPredictor delegate;
    private final DrtWaitingTimeProvider waitingTimeProvider;

    // Simple counter for logging (to verify it's working)
    private static int callCount = 0;

    @Inject
    public LeedsDrtPredictor(DefaultDrtPredictor basePredictor,
            DrtWaitingTimeProvider waitingTimeProvider) {
        this.delegate = basePredictor;
        this.waitingTimeProvider = waitingTimeProvider;
    }

    @Override
    public DrtVariables predictVariables(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {

        // Get base prediction
        DrtVariables baseVars = delegate.predictVariables(person, trip, elements);

        // Extract DRT mode
        String mode = extractDrtMode(elements);

        // Get departure time
        double departureTime = trip.getDepartureTime();

        // Get actual waiting time from provider
        double actualWaitingTime_sec = waitingTimeProvider.getWaitingTime(mode, departureTime);
        double actualWaitingTime_min = actualWaitingTime_sec / 60.0;

        // Log every 1000th call to verify it's working
        callCount++;
        if (callCount % 1000 == 0) {
            log.info("[LeedsDrtPredictor] Call #{}: mode={}, depTime={} ({} hrs), waitTime={} sec ({} min)",
                callCount, mode, (int)departureTime, String.format("%.1f", departureTime/3600.0), 
                String.format("%.1f", actualWaitingTime_sec), String.format("%.1f", actualWaitingTime_min));
        }

        // Return new variables with updated waiting time
        return new DrtVariables(baseVars.travelTime_min, baseVars.cost_MU,
                baseVars.euclideanDistance_km, actualWaitingTime_min, // Use actual waiting time
                baseVars.accessEgressTime_min);
    }

    private String extractDrtMode(List<? extends PlanElement> elements) {
        for (PlanElement element : elements) {
            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                String mode = leg.getMode();
                if (mode.startsWith("drt")) {
                    return mode;
                }
            }
        }
        return "drt"; // Fallback
    }
}
