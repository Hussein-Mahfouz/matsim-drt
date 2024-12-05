package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.CarUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.CarPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.CarVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPredictorUtils;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsCarUtilityEstimator extends CarUtilityEstimator {
	private final LeedsModeParameters parameters;
	private final LeedsSpatialPredictor spatialPredictor;
	private final CarPredictor carPredictor;

	@Inject
	public LeedsCarUtilityEstimator(LeedsModeParameters parameters,
			LeedsSpatialPredictor spatialPredictor, CarPredictor carPredictor) {
		super(parameters, carPredictor);

		this.parameters = parameters;
		this.spatialPredictor = spatialPredictor;
		this.carPredictor = carPredictor;
	}

	// protected double estimateUrbanUtility(LeedsSpatialVariables variables) {
	// double utility = 0.0;

	// if (variables.hasUrbanOrigin && variables.hasUrbanDestination) {
	// utility += parameters.leedsCar.betaInsideUrbanArea;
	// }

	// if (variables.hasUrbanOrigin || variables.hasUrbanDestination) {
	// utility += parameters.leedsCar.betaCrossingUrbanArea;
	// }

	// return utility;
	// }

	@Override
	protected double estimateTravelTimeUtility(CarVariables variables) {
		double lambda = parameters.leedsCar.lambdaTravelTime;
		// box-cox transformation
		return parameters.car.betaTravelTime_u_min
				* ((Math.pow(variables.travelTime_min, lambda) - 1) / lambda);
	}

	@Override
	protected double estimateMonetaryCostUtility(CarVariables variables) {
		// add 0.1 to avoid log(0)
		double travel_cost = (variables.euclideanDistance_km + 0.1) * variables.cost_MU;
		return parameters.betaCost_u_MU * Math.log(travel_cost);
	}

	protected double estimateCommutingUtility(LeedsSpatialVariables variables) {
		return variables.isCommuting ? parameters.leedsCar.betaCommuting : 0.0;
	}


	@Override
	public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		CarVariables carVariables = carPredictor.predictVariables(person, trip, elements);
		LeedsSpatialVariables spatialVariables =
				spatialPredictor.predictVariables(person, trip, elements);

		double utility = 0.0;

		// utility += super.estimateUtility(person, trip, elements);
		// utility += estimateUrbanUtility(spatialVariables);

		utility += estimateConstantUtility();
		utility += estimateTravelTimeUtility(carVariables);
		// utility += estimateMonetaryCostUtility(carVariables);
		utility += estimateCommutingUtility(spatialVariables);

		return utility;
	}
}
