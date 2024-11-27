package com.husseinmahfouz.matsim.dmc.policies;

import org.matsim.core.config.ReflectiveConfigGroup;

public abstract class PolicyConfigGroup extends ReflectiveConfigGroup {
	protected PolicyConfigGroup(String name) {
		super(name);
	}

	@Parameter
	public String policyName;

	@Parameter
	public boolean active = true;

	@Parameter
	public String personFilter;
}