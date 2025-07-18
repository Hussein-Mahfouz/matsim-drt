package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.CachedVariablePredictor;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PredictorUtils;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.TaxiVariables;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import org.matsim.core.router.TripStructureUtils;

import com.google.common.base.Verify;
import com.google.inject.Inject;
import com.google.inject.name.Named;

public class LeedsTaxiPredictor extends CachedVariablePredictor<TaxiVariables> {
	private final CostModel costModel;

	@Inject
	public LeedsTaxiPredictor(@Named("taxi") CostModel costModel) {
		this.costModel = costModel;

	}

	@Override
	public TaxiVariables predict(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {

		double taxiTravelTime_min = 0;
		double accessEgressTime_min = 0;
		// TODO: See https://github.com/Hussein-Mahfouz/matsim-drt/issues/33#issuecomment-2639537703
		double waitingTime_min = 10.0;

		boolean foundTaxi = false;

		for (Leg leg : TripStructureUtils.getLegs(elements)) {
			if (leg.getMode().equals(TransportMode.taxi)) {
				Verify.verify(!foundTaxi);
				taxiTravelTime_min += leg.getTravelTime().seconds() / 60.0;
			} else if (leg.getMode().equals(TransportMode.walk)) {
				accessEgressTime_min += leg.getTravelTime().seconds() / 60.0;
			} else {
				throw new IllegalStateException("Unexpected mode in taxi chain: " + leg.getMode());
			}
		}


		double cost_MU = costModel.calculateCost_MU(person, trip, elements);
		double euclideanDistance_km = PredictorUtils.calculateEuclideanDistance_km(trip);


		return new TaxiVariables(taxiTravelTime_min, cost_MU, euclideanDistance_km,
				accessEgressTime_min, waitingTime_min);
	}
}
