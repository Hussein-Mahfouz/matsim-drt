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
import com.google.inject.Inject;

public class RejectionTracker implements PassengerRequestSubmittedEventHandler, PassengerRequestRejectedEventHandler {
    private static final Logger log = LogManager.getLogger(RejectionTracker.class);

    // ==========================================
    // FOR REJECTION CONSTRAINT (CUMULATIVE)
    // ==========================================
    // Cumulative per-person tracking (NOT reset each iteration)
    private IdMap<Person, Integer> cumulativeRequests = new IdMap<>(Person.class);
    private IdMap<Person, Integer> cumulativeRejections = new IdMap<>(Person.class);

    // Bayesian prior parameters (injected via constructor)
    private final int priorRequests;      // Virtual prior experience
    private final int priorRejections;     // Represents 10% base rejection rate

	// constructor that takes config
	@Inject
    public RejectionTracker(RejectionConstraintConfig config) {
        this.priorRequests = config.getPriorRequests();
        this.priorRejections = config.getPriorRejections();
    }

    // ==========================================
    // FOR PENALTY CONTROLLER (PER-ITERATION)
    // ==========================================
    // Per-iteration per-mode tracking (reset each iteration)
    private Map<String, Integer> requestsByMode = new HashMap<>();
    private Map<String, Integer> rejectionsByMode = new HashMap<>();

    @Override
    public void handleEvent(PassengerRequestRejectedEvent event) {
        String mode = event.getMode();
        
        // Update BOTH tracking systems
        for (Id<Person> personId : event.getPersonIds()) {
            // 1. Cumulative per-person (for constraint)
            cumulativeRejections.compute(personId, (k, v) -> v == null ? 1 : v + 1);
            
            // 2. Per-iteration per-mode (for penalty controller)
            rejectionsByMode.merge(mode, event.getPersonIds().size(), Integer::sum);
        }
    }

    @Override
    public void handleEvent(PassengerRequestSubmittedEvent event) {
        String mode = event.getMode();
        
        // Update BOTH tracking systems
        for (Id<Person> personId : event.getPersonIds()) {
            // 1. Cumulative per-person (for constraint)
            cumulativeRequests.compute(personId, (k, v) -> v == null ? 1 : v + 1);
            
            // 2. Per-iteration per-mode (for penalty controller)
            requestsByMode.merge(mode, event.getPersonIds().size(), Integer::sum);
        }
    }

    // ==========================================
    // FOR REJECTION CONSTRAINT
    // ==========================================
    
    /**
     * Get rejection probability for a specific person (used by RejectionConstraint)
     * Uses Bayesian smoothing with cumulative history across all iterations
     */
    public double getRejectionProbability(Id<Person> personId) {
        int observedRejections = cumulativeRejections.getOrDefault(personId, 0);
        int observedRequests = cumulativeRequests.getOrDefault(personId, 0);
        
        // Bayesian posterior estimate: (prior + observed) / (prior + observed)
        return (double) (priorRejections + observedRejections) 
               / (priorRequests + observedRequests);
    }

    /**
     * Get number of cumulative requests for a person (for minimum threshold check)
     */
    public int getNumberOfRequests(Id<Person> personId) {
        return cumulativeRequests.getOrDefault(personId, 0);
    }

    // ==========================================
    // FOR PENALTY CONTROLLER
    // ==========================================
    
    /**
     * Get rejection rate for specific mode in THIS iteration (used by DrtPenaltyController)
     */
    public double getRejectionRateForMode(String mode) {
        int requests = requestsByMode.getOrDefault(mode, 0);
        int rejections = rejectionsByMode.getOrDefault(mode, 0);
        return requests > 0 ? (double) rejections / requests : 0.0;
    }

    // ==========================================
    // ITERATION END / RESET
    // ==========================================
    
    @Override
    public void reset(int iteration) {
        // Log per-mode summary (from THIS iteration)
        for (String mode : requestsByMode.keySet()) {
            int requests = requestsByMode.getOrDefault(mode, 0);
            int rejections = rejectionsByMode.getOrDefault(mode, 0);
            double rate = requests > 0 ? (double) rejections / requests : 0.0;
            
            log.info("Iteration {}: Mode {} - {} requests, {} rejections (rate: {}%)", 
                iteration, mode, requests, rejections, String.format("%.1f", rate * 100));
        }
        
        // Log cumulative summary (for debugging constraint)
        int totalCumulativeRequests = cumulativeRequests.values().stream()
            .mapToInt(Integer::intValue).sum();
        int totalCumulativeRejections = cumulativeRejections.values().stream()
            .mapToInt(Integer::intValue).sum();
        double cumulativeRate = totalCumulativeRequests > 0 
            ? (double) totalCumulativeRejections / totalCumulativeRequests : 0.0;
        
        log.info("Iteration {}: CUMULATIVE - {} requests, {} rejections (rate: {}%) across {} people", 
                iteration, 
                totalCumulativeRequests, 
                totalCumulativeRejections, 
                String.format("%.1f", cumulativeRate * 100),
                cumulativeRequests.size());
        
        // ONLY reset per-iteration mode stats (used by penalty controller)
        requestsByMode.clear();
        rejectionsByMode.clear();
        
        // DON'T reset cumulative person stats (used by constraint)
        // cumulativeRequests and cumulativeRejections stay cumulative
    }
}