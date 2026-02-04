package com.husseinmahfouz.matsim.drt.rejections;

import org.matsim.api.core.v01.Id;
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
    // FOR REJECTION CONSTRAINT (CUMULATIVE, PER-MODE)
    // ==========================================
    // Cumulative per-person PER-MODE tracking (NOT reset each iteration)
    // Structure: personId -> (baseMode -> count)
    // e.g., person1 -> {drtNE: 5, drtNW: 2}
    private Map<Id<Person>, Map<String, Integer>> cumulativeRequestsByMode = new HashMap<>();
    private Map<Id<Person>, Map<String, Integer>> cumulativeRejectionsByMode = new HashMap<>();

    // Bayesian prior parameters (injected via constructor)
    private final int priorRequests;      // Virtual prior experience
    private final int priorRejections;     // Virtual prior rejections (default: 1, which gives 10% base rate when priorRequests=10)

	// constructor that takes config
	@Inject
    public RejectionTracker(RejectionConstraintConfig config) {
        this.priorRequests = config.getPriorRequests();
        this.priorRejections = config.getPriorRejections();
    }
    
    /**
     * Extract base mode by removing "_feeder" suffix.
     * e.g., "drtNE_feeder" -> "drtNE", "drt_feeder" -> "drt", "drtNE" -> "drtNE"
     */
    private String getBaseMode(String mode) {
        if (mode.endsWith("_feeder")) {
            return mode.substring(0, mode.length() - "_feeder".length());
        }
        return mode;
    }

    // ==========================================
    // FOR PENALTY CONTROLLER (PER-ITERATION)
    // ==========================================
    // Per-iteration per-BASE-mode tracking (reset each iteration)
    // Uses baseMode so drtNE + drtNE_feeder are combined
    private Map<String, Integer> requestsByBaseMode = new HashMap<>();
    private Map<String, Integer> rejectionsByBaseMode = new HashMap<>();

    @Override
    public void handleEvent(PassengerRequestRejectedEvent event) {
        String mode = event.getMode();
        String baseMode = getBaseMode(mode);
        
        // Update BOTH tracking systems
        for (Id<Person> personId : event.getPersonIds()) {
            // 1. Cumulative per-person PER-MODE (for constraint)
            cumulativeRejectionsByMode
                .computeIfAbsent(personId, k -> new HashMap<>())
                .merge(baseMode, 1, Integer::sum);
            
            // 2. Per-iteration per-BASE-mode (for penalty controller)
            rejectionsByBaseMode.merge(baseMode, 1, Integer::sum);
        }
    }

    @Override
    public void handleEvent(PassengerRequestSubmittedEvent event) {
        String mode = event.getMode();
        String baseMode = getBaseMode(mode);
        
        // Update BOTH tracking systems
        for (Id<Person> personId : event.getPersonIds()) {
            // 1. Cumulative per-person PER-MODE (for constraint)
            cumulativeRequestsByMode
                .computeIfAbsent(personId, k -> new HashMap<>())
                .merge(baseMode, 1, Integer::sum);
            
            // 2. Per-iteration per-BASE-mode (for penalty controller)
            requestsByBaseMode.merge(baseMode, 1, Integer::sum);
        }
    }

    // ==========================================
    // FOR REJECTION CONSTRAINT (PER-MODE)
    // ==========================================
    
    /**
     * Get rejection probability for a specific person AND mode (used by RejectionConstraint)
     * Uses Bayesian smoothing with cumulative history across all iterations
     * 
     * @param personId The person to check
     * @param mode The DRT mode being considered (feeder suffix is stripped automatically)
     */
    public double getRejectionProbability(Id<Person> personId, String mode) {
        String baseMode = getBaseMode(mode);
        
        Map<String, Integer> personRejections = cumulativeRejectionsByMode.get(personId);
        Map<String, Integer> personRequests = cumulativeRequestsByMode.get(personId);
        
        int observedRejections = (personRejections != null) ? personRejections.getOrDefault(baseMode, 0) : 0;
        int observedRequests = (personRequests != null) ? personRequests.getOrDefault(baseMode, 0) : 0;
        
        // Bayesian posterior estimate: (prior + observed) / (prior + observed)
        return (double) (priorRejections + observedRejections) 
               / (priorRequests + observedRequests);
    }

    /**
     * Get number of cumulative requests for a person AND mode (for minimum threshold check)
     * 
     * @param personId The person to check
     * @param mode The DRT mode being considered (feeder suffix is stripped automatically)
     */
    public int getNumberOfRequests(Id<Person> personId, String mode) {
        String baseMode = getBaseMode(mode);
        Map<String, Integer> personRequests = cumulativeRequestsByMode.get(personId);
        return (personRequests != null) ? personRequests.getOrDefault(baseMode, 0) : 0;
    }
    
    /**
     * Get total cumulative requests across ALL modes for a person (for logging)
     */
    public int getTotalRequests(Id<Person> personId) {
        Map<String, Integer> personRequests = cumulativeRequestsByMode.get(personId);
        if (personRequests == null) return 0;
        return personRequests.values().stream().mapToInt(Integer::intValue).sum();
    }

    // ==========================================
    // FOR PENALTY CONTROLLER
    // ==========================================
    
    /**
     * Get rejection rate for specific BASE mode in THIS iteration (used by DrtPenaltyController)
     * Automatically combines drtNE + drtNE_feeder into a single rate
     * 
     * @param mode The mode to query (feeder suffix is stripped automatically)
     */
    public double getRejectionRateForMode(String mode) {
        String baseMode = getBaseMode(mode);
        int requests = requestsByBaseMode.getOrDefault(baseMode, 0);
        int rejections = rejectionsByBaseMode.getOrDefault(baseMode, 0);
        return requests > 0 ? (double) rejections / requests : 0.0;
    }

    // ==========================================
    // ITERATION END / RESET
    // ==========================================
    
    @Override
    public void reset(int iteration) {
        // Log per-BASE-mode summary (from THIS iteration) - used by penalty controller
        for (String baseMode : requestsByBaseMode.keySet()) {
            int requests = requestsByBaseMode.getOrDefault(baseMode, 0);
            int rejections = rejectionsByBaseMode.getOrDefault(baseMode, 0);
            double rate = requests > 0 ? (double) rejections / requests : 0.0;
            
            log.info("Iteration {}: Base mode {} - {} requests, {} rejections (rate: {}%)", 
                iteration, baseMode, requests, rejections, String.format("%.1f", rate * 100));
        }
        
        // Log cumulative summary per base mode (for debugging constraint)
        Map<String, int[]> cumulativeByBaseMode = new HashMap<>();
        for (Map.Entry<Id<Person>, Map<String, Integer>> entry : cumulativeRequestsByMode.entrySet()) {
            for (Map.Entry<String, Integer> modeEntry : entry.getValue().entrySet()) {
                String baseMode = modeEntry.getKey();
                int requests = modeEntry.getValue();
                int rejections = cumulativeRejectionsByMode.getOrDefault(entry.getKey(), new HashMap<>())
                    .getOrDefault(baseMode, 0);
                cumulativeByBaseMode.computeIfAbsent(baseMode, k -> new int[2]);
                cumulativeByBaseMode.get(baseMode)[0] += requests;
                cumulativeByBaseMode.get(baseMode)[1] += rejections;
            }
        }
        
        for (Map.Entry<String, int[]> entry : cumulativeByBaseMode.entrySet()) {
            String baseMode = entry.getKey();
            int totalRequests = entry.getValue()[0];
            int totalRejections = entry.getValue()[1];
            double rate = totalRequests > 0 ? (double) totalRejections / totalRequests : 0.0;
            log.info("Iteration {}: CUMULATIVE base mode {} - {} requests, {} rejections (rate: {}%)", 
                iteration, baseMode, totalRequests, totalRejections, String.format("%.1f", rate * 100));
        }
        
        log.info("Iteration {}: CUMULATIVE tracking {} unique people", 
                iteration, cumulativeRequestsByMode.size());
        
        // ONLY reset per-iteration base mode stats (used by penalty controller)
        requestsByBaseMode.clear();
        rejectionsByBaseMode.clear();
        
        // DON'T reset cumulative person stats (used by constraint)
        // cumulativeRequestsByMode and cumulativeRejectionsByMode stay cumulative
    }
}