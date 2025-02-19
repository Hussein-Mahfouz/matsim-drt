package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.predictors.CachedVariablePredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

public class LeedsPersonPredictor extends CachedVariablePredictor<LeedsPersonVariables> {
	@Override
	protected LeedsPersonVariables predict(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		boolean hasSubscription = LeedsPredictorUtils.hasSubscription(person);
		double hhlIncomeSPC = (double) person.getAttributes().getAttribute("hhlIncomeSPC");
		double indIncomeSPC = (double) person.getAttributes().getAttribute("indIncomeSPC");
		int age = (int) person.getAttributes().getAttribute("age");
		boolean isStudent = (boolean) person.getAttributes().getAttribute("isStudent");
		boolean isMale = "male".equals(person.getAttributes().getAttribute("gender"));
		boolean isPassenger = (boolean) person.getAttributes().getAttribute("isPassenger");
		boolean hasLicence = (boolean) person.getAttributes().getAttribute("hasLicence");
		String carAvailability = (String) person.getAttributes().getAttribute("CarAvailability");
		String bikeAvailability = (String) person.getAttributes().getAttribute("BikeAvailability");
		int householdID = (int) person.getAttributes().getAttribute("hid");

		return new LeedsPersonVariables(hasSubscription, hhlIncomeSPC, indIncomeSPC, age, isStudent, isMale,
				isPassenger, hasLicence, carAvailability, bikeAvailability, householdID);
	}
}
