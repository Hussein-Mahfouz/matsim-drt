package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.modes.drt.mode_choice.utilities.estimators.DrtUtilityEstimator;
import org.eqasim.core.simulation.modes.drt.mode_choice.predictors.DrtPredictor;
import org.eqasim.core.simulation.modes.drt.mode_choice.variables.DrtVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;

import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsDrtUtilityEstimator extends DrtUtilityEstimator {
    private final LeedsModeParameters parameters;
    private final LeedsPersonPredictor personPredictor;
    private final DrtPredictor drtPredictor;

    @Inject
    public LeedsDrtUtilityEstimator(LeedsModeParameters parameters,
            // LeedsSpatialPredictor spatialPredictor,
            LeedsPersonPredictor personPredictor, DrtPredictor drtPredictor) {
        super(parameters, drtPredictor);

        this.parameters = parameters;
        // this.spatialPredictor = spatialPredictor;
        this.personPredictor = personPredictor;
        this.drtPredictor = drtPredictor;
    }

    @Override
    protected double estimateTravelTimeUtility(DrtVariables variables) {
        double utility = 0.0;

        utility += parameters.leedsPT.betaInVehicleTimeBus_u_min * variables.travelTime_min;

        return utility;
    }



    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        // LeedsSpatialVariables variables = spatialPredictor.predictVariables(person, trip,
        // elements);
        LeedsPersonVariables variables_person =
                personPredictor.predictVariables(person, trip, elements);
        DrtVariables drtVariables = drtPredictor.predictVariables(person, trip, elements);

        double utility = 0.0;

        utility += estimateConstantUtility();
        utility += estimateTravelTimeUtility(drtVariables);
        utility += estimateWaitingTimeUtility(drtVariables);
        utility += estimateAccessEgressTimeUtility(drtVariables);


        return utility;
    }
}
