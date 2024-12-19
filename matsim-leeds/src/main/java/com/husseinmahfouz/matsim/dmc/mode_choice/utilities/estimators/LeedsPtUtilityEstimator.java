package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.EstimatorUtils;
import org.eqasim.core.simulation.mode_choice.utilities.estimators.PtUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.variables.CarVariables;
import org.eqasim.core.simulation.mode_choice.utilities.variables.PtVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPtPredictor;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPtVariables;

import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsPtUtilityEstimator extends PtUtilityEstimator {
	private final LeedsModeParameters parameters;
	private final LeedsPersonPredictor personPredictor;
	private final LeedsPtPredictor ptPredictor;
	private final LeedsSpatialPredictor spatialPredictor;

	@Inject
	public LeedsPtUtilityEstimator(LeedsModeParameters parameters, LeedsPtPredictor ptPredictor,
			LeedsPersonPredictor personPredictor, LeedsSpatialPredictor spatialPredictor) {
		super(parameters, ptPredictor.delegate);
		this.parameters = parameters;
		this.ptPredictor = ptPredictor;
		this.personPredictor = personPredictor;
		this.spatialPredictor = spatialPredictor;
	}

	// No need to override
	protected double estimateConstantUtility(LeedsPtVariables variables) {
		if (variables.railTravelTime_min > variables.busTravelTime_min) {
			return parameters.leedsPT.alpha_u_Rail;
		} else {
			return parameters.leedsPT.alpha_u_Bus;
		}
	}

	// No need to @Override, as method has different signature (different parameters:
	// LeedsPtVariables instead of PtVariables)
	protected double estimateInVehicleTimeUtility(LeedsPtVariables variables) {
		double lambda = parameters.leedsPT.lambdaTravelTime;
		double utility = 0.0;

		// Rail
		utility += parameters.leedsPT.betaInVehicleTimeRail_u_min * variables.railTravelTime_min;
		// Bus
		// box-cox transformation
		if (variables.busTravelTime_min > 1) {
			utility += parameters.leedsPT.betaInVehicleTimeBus_u_min
					* ((Math.pow(variables.busTravelTime_min, lambda) - 1) / lambda);
		}

		return utility;

	}

	protected double estimateOutOfVehicleTimeUtility(LeedsPtVariables variables) {
		double utility = 0.0;

		double lambda = parameters.leedsPT.lambdaOutofVehicleTime;
		double OVT_rail = variables.railAccessEgressTime_min + variables.railWaitingTime_min;
		double OVT_bus = variables.busAccessEgressTime_min + variables.busWaitingTime_min;
		// Bus and Rail both have box cox transformation for OVT
		// Rail (if OVT_rail == 0, then utility will be positive)
		if (OVT_rail > 1) {
			utility += parameters.leedsPT.betaOutofVehicleTimeRail_u_min
					* ((Math.pow(OVT_rail, lambda) - 1) / lambda);
		}
		// Bus
		if (OVT_bus > 1) {
			utility += parameters.leedsPT.betaOutofVehicleTimeBus_u_min
					* ((Math.pow(OVT_bus, lambda) - 1) / lambda);
		}

		return utility;
	}

	@Override
	protected double estimateMonetaryCostUtility(PtVariables variables) {
		double utility = 0.0;
		double cost = variables.cost_MU;
		// this avoids log(0) which is undefined
		if (cost > 0) {
			utility += parameters.betaCost_u_MU * Math.log(cost);
		}
		return utility;
	}

	protected double estimateAmPmPeakUtility(LeedsSpatialVariables variables,
			LeedsPtVariables variables_pt) {
		double utility = 0.0;



		if (variables.isAMPeak || variables.isPMPeak) {
			if (variables_pt.railTravelTime_min > 0) {
				utility += parameters.leedsPT.betaAmPmPeakRail;
			} else if (variables_pt.busTravelTime_min > 0) {
				utility += parameters.leedsPT.betaAmPmPeakBus;
			}
		}
		return utility;
	}

	@Override
	public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		LeedsPersonVariables variables = personPredictor.predictVariables(person, trip, elements);
		LeedsPtVariables ptVariables = ptPredictor.predictVariables(person, trip, elements);
		LeedsSpatialVariables spatialVariables =
				spatialPredictor.predictVariables(person, trip, elements);

		double utility = 0.0;

		utility += estimateConstantUtility(ptVariables);
		utility += estimateInVehicleTimeUtility(ptVariables);
		utility += estimateOutOfVehicleTimeUtility(ptVariables);
		utility += estimateMonetaryCostUtility(ptVariables);
		utility += estimateAmPmPeakUtility(spatialVariables, ptVariables);

		// utility += estimateLineSwitchUtility(variables_pt);
		// if (variables.hhlIncome == 0.0)
		// utility += estimateMonetaryCostUtility(variables_pt)
		// * (parameters.spAvgHHLIncome.avg_hhl_income / parameters.spAvgHHLIncome.avg_hhl_income);
		// else
		// utility += estimateMonetaryCostUtility(variables_pt)
		// * (parameters.spAvgHHLIncome.avg_hhl_income / parameters.spAvgHHLIncome.avg_hhl_income);

		return utility;
	}
}
