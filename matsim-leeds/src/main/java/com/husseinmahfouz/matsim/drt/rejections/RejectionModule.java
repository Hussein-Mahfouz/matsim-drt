package com.husseinmahfouz.matsim.drt.rejections;

import java.util.Collection;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import org.matsim.contrib.drt.run.DrtConfigGroup;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contribs.discrete_mode_choice.modules.AbstractDiscreteModeChoiceExtension;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.Config;

import org.eqasim.core.simulation.modes.feeder_drt.config.FeederDrtConfigGroup;
import org.eqasim.core.simulation.modes.feeder_drt.config.MultiModeFeederDrtConfigGroup;

import com.google.inject.Inject;
import com.google.inject.Provides;
import com.google.inject.Singleton;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RejectionModule extends AbstractDiscreteModeChoiceExtension {
	private static final Logger log = LogManager.getLogger(RejectionModule.class);
	
	private final CommandLine commandLine;

	public RejectionModule(CommandLine commandLine) {
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

	/**
	 * Auto-detect all DRT modes (standalone + feeder) from the MATSim config.
	 * This ensures the constraint applies to all DRT variants without hardcoding.
	 */
	@Provides
	@Singleton
	public Collection<String> provideDrtModes(Config matsimConfig) {
		Set<String> allDrtModes = new HashSet<>();
		
		// 1. Get standalone DRT modes from MultiModeDrtConfigGroup
		MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(matsimConfig);
		if (multiModeDrtConfig != null) {
			for (DrtConfigGroup drtConfig : multiModeDrtConfig.getModalElements()) {
				allDrtModes.add(drtConfig.getMode());
			}
		}
		
		// 2. Get feeder DRT modes from MultiModeFeederDrtConfigGroup
		MultiModeFeederDrtConfigGroup feederConfig = 
			(MultiModeFeederDrtConfigGroup) matsimConfig.getModules().get(MultiModeFeederDrtConfigGroup.GROUP_NAME);
		if (feederConfig != null) {
			for (FeederDrtConfigGroup feederDrtConfig : feederConfig.getModalElements()) {
				allDrtModes.add(feederDrtConfig.getMode());
			}
		}
		
		log.info("RejectionModule: Auto-detected DRT modes for constraint: {}", allDrtModes);
		return allDrtModes;
	}

	@Provides
	@Singleton
	public RejectionConstraint.Factory provideRejectionConstraintFactory(
			RejectionTracker tracker, 
			RejectionConstraintConfig config,
			Collection<String> drtModes) {
		Random random = new Random();
		return new RejectionConstraint.Factory(tracker, random, drtModes, config);
	}

}