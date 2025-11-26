package com.husseinmahfouz.matsim.drt.rejections;

import java.util.Collection;
import java.util.List;
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

	private final boolean rejected;
	private final Collection<String> modes;

	private RejectionConstraint(boolean rejected, Collection<String> modes) {
		this.modes = modes;
		this.rejected = rejected; 
	}

	@Override
	public boolean validateBeforeEstimation(DiscreteModeChoiceTrip trip, String mode, List<String> previousModes) {
		if (modes.contains(mode)) {
			return !rejected; // Block DRT if person was "rejected"
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
			int attempts = tracker.getNumberOfRequests(person.getId());
            
            // Grace period: allow DRT until person has minimum attempts
            if (attempts < minAttempts) {
                return new RejectionConstraint(false, modes);  // Not rejected
            }
            
            // Apply Bayesian-smoothed rejection probability
            double rejectionProb = tracker.getRejectionProbability(person.getId());
            boolean rejected = random.nextDouble() < rejectionProb;
            
            // Log occasionally for debugging (first few times constraint applies)
            if (attempts >= minAttempts && attempts <= minAttempts + 2) {
                log.info("Person {} has {} cumulative attempts, rejection prob={}%, rejected={}", 
                         person.getId(), attempts, 
                         String.format("%.1f", rejectionProb * 100), rejected);
            }
            
            return new RejectionConstraint(rejected, modes);
        }
    }
}