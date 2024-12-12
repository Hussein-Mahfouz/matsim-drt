package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors;

import org.matsim.api.core.v01.population.Activity;
import org.matsim.api.core.v01.population.Person;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import org.matsim.core.utils.misc.OptionalTime;
import org.matsim.api.core.v01.population.Leg;


public class LeedsPredictorUtils {
	static public boolean hasSubscription(Person person) {
		Boolean hasSubscription =
				(Boolean) person.getAttributes().getAttribute("hasPTSubscription");
		return hasSubscription != null && hasSubscription;
	}

	static public boolean isUrbanArea(Activity activity) {
		Boolean isUrban = (Boolean) activity.getAttributes().getAttribute("isUrban");
		return isUrban != null && isUrban;
	}

	// Check if a trip is a commuting trip
	static public boolean isCommutingTrip(Activity originActivity, Activity destinationActivity) {
		String originType = originActivity.getType();
		String destinationType = destinationActivity.getType();

		return ("home".equals(originType) && "work".equals(destinationType))
				|| ("work".equals(originType) && "home".equals(destinationType));
	}

	// Check if a trip is being made at the am peak
	static public boolean isAmPeakTrip(DiscreteModeChoiceTrip trip) {
		double departureTime = trip.getDepartureTime();
		double startTimeInSeconds = 7 * 3600; // 7 AM in seconds
		double endTimeInSeconds = 10 * 3600; // 10 AM in seconds

		return departureTime >= startTimeInSeconds && departureTime <= endTimeInSeconds;
	}



	// Check if a trip is being made at the pm peak
	static public boolean isPmPeakTrip(DiscreteModeChoiceTrip trip) {
		double departureTime = trip.getDepartureTime();
		double startTimeInSeconds = 16 * 3600; // 4 PM in seconds
		double endTimeInSeconds = 19 * 3600; // 7 PM in seconds

		return departureTime >= startTimeInSeconds && departureTime <= endTimeInSeconds;
	}
}


