package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.PtUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PersonPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PtPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.PtVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsPTUtilityEstimator extends PtUtilityEstimator {
	private final LeedsModeParameters parameters;
	private final LeedsPersonPredictor predictor;
	private final PtPredictor ptPredictor;

	@Inject
	public LeedsPTUtilityEstimator(LeedsModeParameters parameters, PersonPredictor personPredictor,
			PtPredictor ptPredictor, LeedsPersonPredictor predictor) {
		super(parameters, ptPredictor);
		this.ptPredictor = ptPredictor;
		this.parameters = parameters;
		this.predictor = predictor;
	}

	@Override
	protected double estimateInVehicleTimeUtility(PtVariables variables) {
		double lambda = parameters.leedsPT.lambdaTravelTime;
		// box-cox transformation
		return parameters.pt.betaInVehicleTime_u_min
				* ((Math.pow(variables.inVehicleTime_min, lambda) - 1) / lambda);
	}

	protected double estimateOutOfVehicleTimeUtility(PtVariables variables) {
		double lambda = parameters.leedsPT.lambdaOVT;
		// OVT is sum of accessEgressTime_min and waitingTime_min
		double OVT = variables.accessEgressTime_min + variables.waitingTime_min;
		// box-cox transformation
		return parameters.leedsPT.OutofVehicleTime_u_min * ((Math.pow(OVT, lambda) - 1) / lambda);
	}

	@Override
	protected double estimateMonetaryCostUtility(PtVariables variables) {
		return parameters.betaCost_u_MU * Math.log(variables.cost_MU);
	}

	@Override
	public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		LeedsPersonVariables variables = predictor.predictVariables(person, trip, elements);
		PtVariables variables_pt = ptPredictor.predict(person, trip, elements);

		double utility = 0.0;

		utility += estimateConstantUtility();
		utility += estimateInVehicleTimeUtility(variables_pt);
		utility += estimateOutOfVehicleTimeUtility(variables_pt);
		// utility += estimateLineSwitchUtility(variables_pt);
		utility += estimateMonetaryCostUtility(variables_pt);
		// if (variables.hhlIncome == 0.0)
		// utility += estimateMonetaryCostUtility(variables_pt)
		// * (parameters.spAvgHHLIncome.avg_hhl_income / parameters.spAvgHHLIncome.avg_hhl_income);
		// else
		// utility += estimateMonetaryCostUtility(variables_pt)
		// * (parameters.spAvgHHLIncome.avg_hhl_income / parameters.spAvgHHLIncome.avg_hhl_income);

		return utility;
	}
}
