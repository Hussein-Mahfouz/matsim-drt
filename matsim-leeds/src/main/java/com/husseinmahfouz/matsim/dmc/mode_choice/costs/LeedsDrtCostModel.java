package com.husseinmahfouz.matsim.dmc.mode_choice.costs;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.cost.AbstractCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsDrtCostModel extends AbstractCostModel {
	private final LeedsCostParameters costParameters;

	@Inject
	public LeedsDrtCostModel(LeedsCostParameters costParameters) {
		super("drt");
		this.costParameters = costParameters;
	}

	@Override
	public double calculateCost_MU(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		double tripDistance_km = getInVehicleDistance_km(elements);
		// add 0.1 to avoid log(0) in DrtUtilityEstimator: Math.log(variables.cost_MU)
		return (costParameters.drtFareBase + costParameters.drtFarePerKm * tripDistance_km) + 0.1;
	}
}
