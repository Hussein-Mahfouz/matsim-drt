package com.husseinmahfouz.matsim.drt.rejections;

public class RejectionConstraintConfig {
    // Bayesian prior parameters
    private int priorRequests = 10;      // Default: 10 virtual prior attempts
    private int priorRejections = 1;     // Default: 1 virtual rejection (10% base rate)
    
    // Minimum attempts before constraint applies
    private int minAttempts = 3;         // Default: 3 attempts grace period
    
    public int getPriorRequests() {
        return priorRequests;
    }
    
    public void setPriorRequests(int priorRequests) {
        this.priorRequests = priorRequests;
    }
    
    public int getPriorRejections() {
        return priorRejections;
    }
    
    public void setPriorRejections(int priorRejections) {
        this.priorRejections = priorRejections;
    }
    
    public int getMinAttempts() {
        return minAttempts;
    }
    
    public void setMinAttempts(int minAttempts) {
        this.minAttempts = minAttempts;
    }
}