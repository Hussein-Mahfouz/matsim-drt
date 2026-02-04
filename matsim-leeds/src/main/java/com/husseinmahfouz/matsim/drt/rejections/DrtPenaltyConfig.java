package com.husseinmahfouz.matsim.drt.rejections;

public class DrtPenaltyConfig {
    private boolean enabled = false; // Disabled by default
    private double targetRejectionRate = 0.03; // 3% default (ρ* in paper)
    private double controllerGain = 1.0; // K (tune this!)

    public boolean isEnabled() {
        return enabled;
    }
    
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }
    
    public double getTargetRejectionRate() {
        return targetRejectionRate;
    }
    
    public void setTargetRejectionRate(double rate) {
        this.targetRejectionRate = rate;
    }
    
    public double getControllerGain() {
        return controllerGain;
    }
    
    public void setControllerGain(double gain) {
        this.controllerGain = gain;
    }
}