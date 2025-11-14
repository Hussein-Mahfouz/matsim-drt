package com.husseinmahfouz.matsim.drt.rejections;

public class DrtPenaltyConfig {
    private double targetRejectionRate = 0.05; // 5% default
    private double controllerGain = 1.0; // K (tune this!)
    
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