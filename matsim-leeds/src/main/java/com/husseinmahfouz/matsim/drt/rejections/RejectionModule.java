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

}