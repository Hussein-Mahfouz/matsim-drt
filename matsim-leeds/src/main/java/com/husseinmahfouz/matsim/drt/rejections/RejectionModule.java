package com.husseinmahfouz.matsim.drt.rejections;

import java.util.Collection;
import java.util.Random;

import org.matsim.contribs.discrete_mode_choice.modules.AbstractDiscreteModeChoiceExtension;
import org.matsim.core.config.CommandLine;


import com.google.inject.Provides;
import com.google.inject.Singleton;

public class RejectionModule extends AbstractDiscreteModeChoiceExtension {
	private final Collection<String> modes;
	private final CommandLine commandLine;

	public RejectionModule(Collection<String> modes, CommandLine commandLine) {
		this.modes = modes;
		this.commandLine = commandLine;
	}

	@Override
	protected void installExtension() {
		addEventHandlerBinding().to(RejectionTracker.class);
		bindTripConstraintFactory(RejectionConstraint.NAME).to(RejectionConstraint.Factory.class);

    	addControlerListenerBinding().to(DrtPenaltyController.class);
	}

	@Provides
	@Singleton
	public RejectionTracker provideRejectionTracker() {
		return new RejectionTracker();
	}

	@Provides
	@Singleton
	public RejectionConstraint.Factory provideRejectionConstraintFactory(RejectionTracker tracker) {
		Random random = new Random();
		return new RejectionConstraint.Factory(tracker, random, modes);
	}

	@Provides
	@Singleton
	public DrtPenaltyController providePenaltyController(
			RejectionTracker tracker, DrtPenaltyConfig config) {
		return new DrtPenaltyController(tracker, config);	
	}

	@Provides
    @Singleton
    public DrtPenaltyConfig providePenaltyConfig() {
        DrtPenaltyConfig config = new DrtPenaltyConfig();
        
        // READ FROM COMMAND LINE (with fallback to defaults)
        try {
            if (commandLine != null && commandLine.hasOption("target-rejection-rate")) {
                double targetRate = Double.parseDouble(
                    commandLine.getOptionStrict("target-rejection-rate"));
                config.setTargetRejectionRate(targetRate);
            }
            
            if (commandLine != null && commandLine.hasOption("controller-gain")) {
                double gain = Double.parseDouble(
                    commandLine.getOptionStrict("controller-gain"));
                config.setControllerGain(gain);
            }
        } catch (Exception e) {
            // If parsing fails, just use defaults
            System.err.println("Warning: Could not parse DRT penalty parameters, using defaults: " + e.getMessage());
        }
        
        return config;
    }
}