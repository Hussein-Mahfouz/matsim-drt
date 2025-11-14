package com.husseinmahfouz.matsim.drt.rejections;

import org.matsim.core.controler.events.IterationEndsEvent;
import org.matsim.core.controler.listener.IterationEndsListener;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Singleton
public class DrtPenaltyController implements IterationEndsListener {
    
    private static final Logger log = LogManager.getLogger(DrtPenaltyController.class);
    
    private final RejectionTracker rejectionTracker;
    private final double targetRejectionRate; // ρ*
    private final double controllerGain; // K (proportional gain)
    
    private double currentPenalty = 0.0; // π_i (starts at 0)
    
    @Inject
    public DrtPenaltyController(RejectionTracker rejectionTracker, 
                               DrtPenaltyConfig config) {
        this.rejectionTracker = rejectionTracker;
        this.targetRejectionRate = config.getTargetRejectionRate();
        this.controllerGain = config.getControllerGain();
        
        log.info("DrtPenaltyController initialized: target={}, gain={}", 
                String.format("%.1f", targetRejectionRate * 100), controllerGain);
    }
    
    @Override
    public void notifyIterationEnds(IterationEndsEvent event) {
        int iteration = event.getIteration();
        
        // Calculate actual rejection rate for this iteration
        double actualRejectionRate = calculateAverageRejectionRate();
        
        // Calculate error: ε_i = ρ_i - ρ*
        double error = actualRejectionRate - targetRejectionRate;
        
        // Update penalty using proportional control:
        // π_{i+1} = π_i - K * max(0, error)
        // (only increase penalty if actual > target)
        if (error > 0) {
            double previousPenalty = currentPenalty;
            currentPenalty = currentPenalty - controllerGain * error;
            
            log.info("Iteration {}: actualRate={:.1f}%, targetRate={:.1f}%, error={:.3f}, penalty: {:.3f} → {:.3f}",
                    iteration, 
                    actualRejectionRate * 100, 
                    targetRejectionRate * 100,
                    error, 
                    previousPenalty, 
                    currentPenalty);
        } else {
            log.info("Iteration {}: actualRate={:.1f}%, targetRate={:.1f}%, penalty unchanged: {:.3f}",
                    iteration, 
                    actualRejectionRate * 100, 
                    targetRejectionRate * 100, 
                    currentPenalty);
        }
    }
    
    /**
     * Calculate average rejection rate across all persons who used DRT
     */
    private double calculateAverageRejectionRate() {
        return rejectionTracker.getAverageRejectionRate();
    }
    
    /**
     * Get current penalty value (called by utility estimator)
     */
    public double getCurrentPenalty() {
        return currentPenalty;
    }
}