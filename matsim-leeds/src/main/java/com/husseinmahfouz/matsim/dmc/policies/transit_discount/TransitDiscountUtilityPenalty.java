package com.husseinmahfouz.matsim.dmc.policies.transit_discount;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.cost.CostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.policies.PolicyPersonFilter;
import com.husseinmahfouz.matsim.dmc.policies.mode_choice.UtilityPenalty;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

public class TransitDiscountUtilityPenalty implements UtilityPenalty {
	private final CostModel costModel;
	private final LeedsModeParameters modeParameters;
	private final double costFactor;
	private final PolicyPersonFilter personFilter;

	public TransitDiscountUtilityPenalty(CostModel costModel, LeedsModeParameters modeParameters,
			double costFactor, PolicyPersonFilter personFilter) {
		this.costModel = costModel;
		this.modeParameters = modeParameters;
		this.costFactor = costFactor;
		this.personFilter = personFilter;
	}

	@Override
	public double calculatePenalty(String mode, Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		if (mode.equals(TransportMode.pt) && personFilter.applies(person.getId())) {
			double initialCost = costModel.calculateCost_MU(person, trip, elements);
			double updatedCost = initialCost * costFactor;
			return modeParameters.betaCost_u_MU * (updatedCost - initialCost);
		} else {
			return 0.0;
		}
	}
}
