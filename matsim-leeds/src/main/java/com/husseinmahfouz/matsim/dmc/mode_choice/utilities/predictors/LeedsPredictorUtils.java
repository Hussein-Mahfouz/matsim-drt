package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import org.matsim.api.core.v01.population.Activity;
import org.matsim.api.core.v01.population.Person;

public class LeedsPredictorUtils {
	static public boolean hasSubscription(Person person) {
		Boolean hasSubscription =
				(Boolean) person.getAttributes().getAttribute("hasPtSubscription");
		return hasSubscription != null && hasSubscription;
	}

	static public boolean isUrbanArea(Activity activity) {
		Boolean isUrban = (Boolean) activity.getAttributes().getAttribute("isUrban");
		return isUrban != null && isUrban;
	}

	// Check if a trip is a commuting trip
	static public boolean isCommutingTrip(Activity activity) {
		String type = (String) activity.getAttributes().getAttribute("type");
		return "work".equals(type);
	}
}
