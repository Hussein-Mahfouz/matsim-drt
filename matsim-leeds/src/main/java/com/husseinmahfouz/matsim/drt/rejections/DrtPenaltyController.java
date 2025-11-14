package com.husseinmahfouz.matsim.drt.rejections;

import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.drt.run.DrtConfigGroup;
import org.matsim.core.config.Config;
import org.matsim.core.controler.events.IterationEndsEvent;
import org.matsim.core.controler.events.StartupEvent;
import org.matsim.core.controler.listener.IterationEndsListener;
import org.matsim.core.controler.listener.StartupListener;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


@Singleton
public class DrtPenaltyController implements IterationEndsListener, StartupListener {
    
    private static final Logger log = LogManager.getLogger(DrtPenaltyController.class);
    
    private final RejectionTracker rejectionTracker;
    private final double targetRejectionRate; // ρ*
    private final double controllerGain; // K (proportional gain)
    private final Set<String> drtModes; // Auto-detected from config

    private Map<String, Double> currentPenaltyByMode = new HashMap<>();
        
    @Inject
    public DrtPenaltyController(RejectionTracker rejectionTracker, 
                               DrtPenaltyConfig config,
                               Config matsimConfig) {
        this.rejectionTracker = rejectionTracker;
        this.targetRejectionRate = config.getTargetRejectionRate();
        this.controllerGain = config.getControllerGain();
        
        // Auto-detect DRT modes (same pattern as DrtWaitingTimeProvider)
        MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(matsimConfig);
        this.drtModes = multiModeDrtConfig.getModalElements().stream()
                .map(DrtConfigGroup::getMode)
                .collect(Collectors.toSet());
        
        // Initialize penalties
        for (String mode : drtModes) {
            currentPenaltyByMode.put(mode, 0.0);
        }
        
    }

    // log when MATSim is ready
    @Override
    public void notifyStartup(StartupEvent event) {
        log.info("DrtPenaltyController initialized: target={}%, gain={}, modes={}", 
                String.format("%.1f", targetRejectionRate * 100), 
                controllerGain, 
                drtModes);
    }

    @Override
    public void notifyIterationEnds(IterationEndsEvent event) {
        int iteration = event.getIteration();
        
        for (String mode : drtModes) {
            double actualRate = rejectionTracker.getRejectionRateForMode(mode);         // Calculate actual rejection rate for this iteration
            double error = actualRate - targetRejectionRate;         // Calculate error: ε_i = ρ_i - ρ*
            double previousPenalty = currentPenaltyByMode.get(mode);
            
            // Update penalty using proportional control:
            // π_{i+1} = π_i - K * max(0, error)
            // (only increase penalty if actual > target)
            if (error > 0) {
            double newPenalty = previousPenalty - controllerGain * error;
            currentPenaltyByMode.put(mode, newPenalty);
            
            log.info("Iteration {}: Mode {} - actualRate={}%, error={}, penalty: {} → {}",
                    iteration, 
                    mode, 
                    String.format("%.1f", actualRate * 100),
                    String.format("%.3f", error),
                    String.format("%.3f", previousPenalty),
                    String.format("%.3f", newPenalty));
            } else {
                log.info("Iteration {}: Mode {} - actualRate={}%, penalty unchanged: {}",
                        iteration, 
                        mode,
                        String.format("%.1f", actualRate * 100),
                        String.format("%.3f", previousPenalty));
            }
        }
    }
    
   /**
     * Get current penalty value (called by utility estimator)
     */
    public double getCurrentPenalty(String mode) {
        return currentPenaltyByMode.getOrDefault(mode, 0.0);
    }

    
}