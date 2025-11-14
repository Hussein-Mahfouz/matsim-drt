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

        DrtVariables baseVars = delegate.predictVariables(person, trip, elements);

        // Try to extract actual DRT mode; if none exists, fall back to base variables
        String actualDrtMode = extractDrtMode(elements);
        
        if (actualDrtMode == null) {
            // No DRT leg found - this can happen for failed routing or feeder modes
            // Just return the base variables (which likely have default/unrealistic values)
            log.debug("No DRT leg found for trip mode={}, using base predictor values", 
                     trip.getInitialMode());
            return baseVars;
        }

        double departureTime = trip.getDepartureTime();
        double actualWaitingTime_sec =
                waitingTimeProvider.getWaitingTime(actualDrtMode, departureTime);
        double actualWaitingTime_min = actualWaitingTime_sec / 60.0;

        callCount++;
        if (callCount % 250 == 0) {
            log.info(
                    "[LeedsDrtPredictor] Call #{}: abstractMode={}, actualMode={}, depTime={} ({} hrs), waitTime={} sec ({} min)",
                    callCount, trip.getInitialMode(), actualDrtMode, (int) departureTime,
                    String.format("%.1f", departureTime / 3600.0),
                    String.format("%.1f", actualWaitingTime_sec),
                    String.format("%.1f", actualWaitingTime_min));
        }

        return new DrtVariables(baseVars.travelTime_min, baseVars.cost_MU,
                baseVars.euclideanDistance_km, actualWaitingTime_min,
                baseVars.accessEgressTime_min);
    }

    /**
     * Extracts the DRT mode from routed trip elements.
     * Returns null if no DRT leg is found (e.g., for failed routing).
     */
    private String extractDrtMode(List<? extends PlanElement> elements) {
        // First try: look for actual DRT leg mode
        for (PlanElement element : elements) {
            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                String mode = leg.getMode();
                
                if (mode.endsWith("_feeder")) {
                    mode = mode.replace("_feeder", "");
                }
                
                if (mode.startsWith("drt")) {
                    return mode;
                }
            }
        }

        // Second try: check routingMode attribute
        for (PlanElement element : elements) {
            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                Object routingModeAttr = leg.getAttributes().getAttribute("routingMode");
                if (routingModeAttr != null) {
                    String routingMode = routingModeAttr.toString();

                    if (routingMode.endsWith("_feeder")) {
                        routingMode = routingMode.replace("_feeder", "");
                    }

                    if (routingMode.startsWith("drt")) {
                        return routingMode;
                    }
                }
            }
        }

        // No DRT mode found - return null instead of throwing exception
        return null;
    }
}