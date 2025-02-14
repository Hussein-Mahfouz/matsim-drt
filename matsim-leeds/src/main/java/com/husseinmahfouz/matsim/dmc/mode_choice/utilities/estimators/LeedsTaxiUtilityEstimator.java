package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.EstimatorUtils;
import org.eqasim.core.simulation.mode_choice.utilities.UtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PersonPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.CarVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.TaxiVariables;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPredictorUtils;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsTaxiPredictor;

import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;


public class LeedsTaxiUtilityEstimator implements UtilityEstimator {
	private final LeedsModeParameters parameters;
	private final LeedsPersonPredictor predictor;
	private final LeedsTaxiPredictor taxiPredictor;
	private final LeedsSpatialPredictor spatialPredictor;

	@Inject
	public LeedsTaxiUtilityEstimator(LeedsModeParameters parameters,
			PersonPredictor personPredictor, LeedsTaxiPredictor taxiPredictor,
			LeedsPersonPredictor predictor, LeedsSpatialPredictor spatialPredictor) {
		this.taxiPredictor = taxiPredictor;
		this.parameters = parameters;
		this.predictor = predictor;
		this.spatialPredictor = spatialPredictor;
	}

	protected double estimateConstantUtility() {
		return parameters.leedsTaxi.alpha_u;
	}

	protected double estimateGenderUtility(LeedsPersonVariables variables) {
		double utility = 0.0;

		if (variables.isMale) {
			utility += parameters.leedsTaxi.betaMale;
		}

		return utility;
	}

	protected double estimateAgeUtility(LeedsPersonVariables variables) {
		double utility = 0.0;

		// Example logic based on age
		if (variables.age >= 18 && variables.age <= 24) {
			utility += parameters.leedsTaxi.betaAge18to24;
		} else if (variables.age >= 25 && variables.age <= 29) {
			utility += parameters.leedsTaxi.betaAge25to29;
		}

		return utility;
	}

	// protected double estimateEducationUtility(LeedsPersonVariables variables) {
	// }

	protected double estimateIncomeUtility(LeedsPersonVariables variables) {
		double utility = 0.0;
		double thresholdIncomeLower = 40000.0;
		double thresholdIncomeUpper = 50000.0;

		if (variables.indIncomeSPC >= thresholdIncomeLower
				&& variables.indIncomeSPC <= thresholdIncomeUpper) {
			utility += parameters.leedsTaxi.betaIncome40kto50k;
		}

		return utility;
	}


	protected double estimateTravelTimeUtility(TaxiVariables variables) {
		double lambda = parameters.leedsTaxi.lambdaTravelTime;
		// in our choice model, there is only one beta (total travel time)
		double totalTravelTime = variables.travelTime_min + variables.accessEgressTime_min
				+ variables.waitingTime_min;
		// box-cox transformation
		return parameters.leedsTaxi.betaTravelTime_u_min
				* ((Math.pow(totalTravelTime, lambda) - 1) / lambda);
	}

	protected double estimateAmPeakUtility(LeedsSpatialVariables variables) {
		return variables.isAMPeak ? parameters.leedsTaxi.betaAmPeak : 0.0;
	}

	protected double estimatePmPeakUtility(LeedsSpatialVariables variables) {
		return variables.isPMPeak ? parameters.leedsTaxi.betaPmPeak : 0.0;
	}

	protected double estimateMonetaryCostUtility(TaxiVariables variables) {
		return parameters.betaCost_u_MU * Math.log(variables.cost_MU);
	}



	@Override
	public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		LeedsPersonVariables variables = predictor.predictVariables(person, trip, elements);
		TaxiVariables variables_taxi = taxiPredictor.predict(person, trip, elements);
		LeedsSpatialVariables spatialVariables =
				spatialPredictor.predictVariables(person, trip, elements);

		double utility = 0.0;

		utility += estimateConstantUtility();
		utility += estimateGenderUtility(variables);
		utility += estimateAgeUtility(variables);
		utility += estimateTravelTimeUtility(variables_taxi);
		utility += estimateAmPeakUtility(spatialVariables);
		utility += estimatePmPeakUtility(spatialVariables);
		utility += estimateMonetaryCostUtility(variables_taxi);
		// TODO:
		// utility += estimateEducationUtility(variables);
		utility += estimateIncomeUtility(variables);

		return utility;
	}


}
