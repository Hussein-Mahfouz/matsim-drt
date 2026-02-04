package com.husseinmahfouz.matsim.drt.rejections;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.matsim.api.core.v01.population.Person;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;
import org.matsim.contribs.discrete_mode_choice.model.constraints.AbstractTripConstraint;
import org.matsim.contribs.discrete_mode_choice.model.trip_based.TripConstraint;
import org.matsim.contribs.discrete_mode_choice.model.trip_based.TripConstraintFactory;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RejectionConstraint extends AbstractTripConstraint {
	public final static String NAME = "RejectionConstraint";

	// Map of mode -> whether that mode is blocked for this person
	private final Map<String, Boolean> rejectedByMode;
	private final Collection<String> modes;

	private RejectionConstraint(Map<String, Boolean> rejectedByMode, Collection<String> modes) {
		this.modes = modes;
		this.rejectedByMode = rejectedByMode; 
	}

	@Override
	public boolean validateBeforeEstimation(DiscreteModeChoiceTrip trip, String mode, List<String> previousModes) {
		if (modes.contains(mode)) {
			// Check if THIS specific mode is blocked
			Boolean rejected = rejectedByMode.getOrDefault(mode, false);
			return !rejected; // Block DRT if person was "rejected" for this mode
		}

		return true; // Allow all other modes
	}

	public static class Factory implements TripConstraintFactory {

		private static final Logger log = LogManager.getLogger(Factory.class);

		private final RejectionTracker tracker;
		private final Random random;
		private final Collection<String> modes;
		private final int minAttempts;


		public Factory(RejectionTracker tracker, Random random, Collection<String> modes, RejectionConstraintConfig config) {
			this.tracker = tracker;
			this.random = random;
			this.modes = modes;
			this.minAttempts = config.getMinAttempts();
		}

		@Override
		public TripConstraint createConstraint(Person person, List<DiscreteModeChoiceTrip> planTrips,
				Collection<String> availableModes) {
			
			// Evaluate rejection probability SEPARATELY for each DRT mode
			Map<String, Boolean> rejectedByMode = new HashMap<>();
			
			for (String mode : modes) {
				int attempts = tracker.getNumberOfRequests(person.getId(), mode);
				
				// Grace period: allow DRT until person has minimum attempts FOR THIS MODE
				if (attempts < minAttempts) {
					rejectedByMode.put(mode, false);  // Not rejected
					continue;
				}
				
				// Apply Bayesian-smoothed rejection probability FOR THIS MODE
				double rejectionProb = tracker.getRejectionProbability(person.getId(), mode);
				boolean rejected = random.nextDouble() < rejectionProb;
				rejectedByMode.put(mode, rejected);
				
				// Log occasionally for debugging (first few times constraint applies for this mode)
				if (attempts >= minAttempts && attempts <= minAttempts + 2) {
					log.info("Person {} mode {}: {} attempts, rejection prob={}%, rejected={}", 
							 person.getId(), mode, attempts, 
							 String.format("%.1f", rejectionProb * 100), rejected);
				}
			}
            
            return new RejectionConstraint(rejectedByMode, modes);
        }
    }
}