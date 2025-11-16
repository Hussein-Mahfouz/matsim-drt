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

	}

	// Provide config from command line
    @Provides
    @Singleton
    public RejectionConstraintConfig provideRejectionConstraintConfig() {
        RejectionConstraintConfig config = new RejectionConstraintConfig();
        
        try {
            if (commandLine != null && commandLine.hasOption("prior-requests")) {
                config.setPriorRequests(
                    Integer.parseInt(commandLine.getOptionStrict("prior-requests")));
            }
            
            if (commandLine != null && commandLine.hasOption("prior-rejections")) {
                config.setPriorRejections(
                    Integer.parseInt(commandLine.getOptionStrict("prior-rejections")));
            }
            
            if (commandLine != null && commandLine.hasOption("min-attempts")) {
                config.setMinAttempts(
                    Integer.parseInt(commandLine.getOptionStrict("min-attempts")));
            }
        } catch (Exception e) {
            System.err.println("Warning: Could not parse rejection constraint parameters: " + e.getMessage());
        }
        
        return config;
    }

	@Provides
	@Singleton
	public RejectionTracker provideRejectionTracker(RejectionConstraintConfig config) {
		return new RejectionTracker(config);
	}

	@Provides
	@Singleton
	public RejectionConstraint.Factory provideRejectionConstraintFactory(RejectionTracker tracker, RejectionConstraintConfig config) {
		Random random = new Random();
		return new RejectionConstraint.Factory(tracker, random, modes, config);
	}

}