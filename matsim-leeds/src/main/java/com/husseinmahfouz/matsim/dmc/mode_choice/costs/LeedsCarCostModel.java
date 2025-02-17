package com.husseinmahfouz.matsim.dmc.mode_choice.costs;


import java.util.List;

import org.eqasim.core.simulation.mode_choice.cost.AbstractCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsCarCostModel extends AbstractCostModel {
	private final LeedsCostParameters costParameters;

	@Inject
	public LeedsCarCostModel(LeedsCostParameters costParameters) {
		super("car");
		this.costParameters = costParameters;
	}

	@Override
	public double calculateCost_MU(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		// add 0.1 to avoid log(0) in CarUtilityEstimator: Math.log(variables.cost_MU)
		return (costParameters.carCost_EUR_km * getInVehicleDistance_km(elements)) + 0.1;
	}
}
