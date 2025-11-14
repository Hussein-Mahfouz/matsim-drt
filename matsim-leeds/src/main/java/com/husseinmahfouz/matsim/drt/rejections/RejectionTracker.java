package com.husseinmahfouz.matsim.drt.rejections;

import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.IdMap;
import org.matsim.api.core.v01.population.Person;
import org.matsim.contrib.dvrp.passenger.PassengerRequestRejectedEvent;
import org.matsim.contrib.dvrp.passenger.PassengerRequestRejectedEventHandler;
import org.matsim.contrib.dvrp.passenger.PassengerRequestSubmittedEvent;
import org.matsim.contrib.dvrp.passenger.PassengerRequestSubmittedEventHandler;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.Map;

public class RejectionTracker implements PassengerRequestSubmittedEventHandler, PassengerRequestRejectedEventHandler {
	private static final Logger log = LogManager.getLogger(RejectionTracker.class);

	private IdMap<Person, Integer> numberOfRequests = new IdMap<>(Person.class);
	private IdMap<Person, Integer> numberOfRejections = new IdMap<>(Person.class);

	// Per-mode tracking (for penalty controller)
    private Map<String, Integer> requestsByMode = new HashMap<>();
    private Map<String, Integer> rejectionsByMode = new HashMap<>();


	private int defaultRequests = 10;
	private int defaultRejections = 0;

	@Override
	public void handleEvent(PassengerRequestRejectedEvent event) {

		// Get mode from event (MATSim already tracks this!)
        String mode = event.getMode();
		// Track per person
		for (Id<Person> personId : event.getPersonIds()) {
			numberOfRejections.compute(personId, (k, v) -> v == null ? defaultRejections + 1 : v + 1);
		}
		// Track per mode
        rejectionsByMode.merge(mode, event.getPersonIds().size(), Integer::sum);
	}

	@Override
	public void handleEvent(PassengerRequestSubmittedEvent event) {
		// Get mode from event
        String mode = event.getMode();
		// Track per person
		for (Id<Person> personId : event.getPersonIds()) {
			numberOfRequests.compute(personId, (k, v) -> v == null ? defaultRequests + 1 : v + 1);
		}
		// Track per mode
        requestsByMode.merge(mode, event.getPersonIds().size(), Integer::sum);
	}

	public double getRejectionProbability(Id<Person> personId) {
		return numberOfRejections.getOrDefault(personId, defaultRejections)
				/ numberOfRequests.getOrDefault(personId, defaultRequests);
	}

	// Get rejection rate for specific mode
    public double getRejectionRateForMode(String mode) {
        int requests = requestsByMode.getOrDefault(mode, 0);
        int rejections = rejectionsByMode.getOrDefault(mode, 0);
        return requests > 0 ? (double) rejections / requests : 0.0;
    }

	@Override
    public void reset(int iteration) {
        // Log per-mode summary
        for (String mode : requestsByMode.keySet()) {
            int requests = requestsByMode.getOrDefault(mode, 0);
            int rejections = rejectionsByMode.getOrDefault(mode, 0);
            double rate = requests > 0 ? (double) rejections / requests : 0.0;
            
            log.info("Iteration {}: Mode {} - {} requests, {} rejections (rate: {:.1f}%)", 
                    iteration, mode, requests, rejections, rate * 100);
        }
        
        // Reset
        numberOfRequests.clear();
        numberOfRejections.clear();
        requestsByMode.clear();
        rejectionsByMode.clear();
    }

	public double getAverageRejectionRate() {
		if (numberOfRequests.isEmpty()) {
			return 0.0;
		}
		
		int totalRequests = numberOfRequests.values().stream()
			.mapToInt(Integer::intValue).sum();
		int totalRejections = numberOfRejections.values().stream()
			.mapToInt(Integer::intValue).sum();
		
		return totalRequests > 0 ? (double) totalRejections / totalRequests : 0.0;
	}
}

