package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.modes.drt.mode_choice.utilities.estimators.DrtUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.variables.PtVariables;
import org.eqasim.core.simulation.modes.drt.mode_choice.variables.DrtVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsDrtPredictor;  
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.drt.rejections.DrtPenaltyController;

import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsDrtUtilityEstimator extends DrtUtilityEstimator {
    private final LeedsModeParameters parameters;
    private final LeedsPersonPredictor personPredictor;
    private final LeedsDrtPredictor drtPredictor;
    private final DrtPenaltyController penaltyController;

    @Inject
    public LeedsDrtUtilityEstimator(LeedsModeParameters parameters,
            // LeedsSpatialPredictor spatialPredictor,
            LeedsPersonPredictor personPredictor, LeedsDrtPredictor drtPredictor, 
            DrtPenaltyController penaltyController) {
        super(parameters, drtPredictor.delegate);

        this.parameters = parameters;
        // this.spatialPredictor = spatialPredictor;
        this.personPredictor = personPredictor;
        this.drtPredictor = drtPredictor;
        this.penaltyController = penaltyController;
    }

    @Override
    protected double estimateTravelTimeUtility(DrtVariables variables) {
        double utility = 0.0;

        // box-cox transformation (same as in LeedsPtUtilityEstimator)
        double lambda = parameters.leedsPT.lambdaTravelTime;
        utility += parameters.drt.betaTravelTime_u_min
                * ((Math.pow(variables.travelTime_min, lambda) - 1) / lambda);

        return utility;
    }

    protected double estimateOutOfVehicleTimeUtility(DrtVariables variables) {
        double utility = 0.0;
        double lambda = parameters.leedsPT.lambdaOutofVehicleTime;

        double OVT = variables.waitingTime_min + variables.accessEgressTime_min;

        if (OVT > 1) {
            utility += parameters.drt.betaWaitingTime_u_min
                    * ((Math.pow(OVT, lambda) - 1) / lambda);
        }

        return utility;
    }

    @Override
	protected double estimateMonetaryCostUtility(DrtVariables variables) {
		double utility = 0.0;
		double cost = variables.cost_MU;
		// this avoids log(0) which is undefined
		if (cost > 0) {
			utility += parameters.betaCost_u_MU * Math.log(cost);
		}
		return utility;
	}

    protected double estimateRejectionPenalty(DiscreteModeChoiceTrip trip) {
        String mode = trip.getInitialMode(); // e.g., "drtNE" or "drtNW"
        double penalty = penaltyController.getCurrentPenalty(mode);
        return parameters.leedsDrt.betaRejectionPenalty_u * penalty;
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
        // utility += estimateWaitingTimeUtility(drtVariables);
        // utility += estimateAccessEgressTimeUtility(drtVariables);
        utility += estimateOutOfVehicleTimeUtility(drtVariables);
        utility += estimateMonetaryCostUtility(drtVariables);
        utility += estimateRejectionPenalty();

        return utility;
    }
}
