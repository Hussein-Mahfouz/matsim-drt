package com.husseinmahfouz.matsim.dmc.policies;

public interface PolicyFactory {
	Policy createPolicy(String name, PolicyPersonFilter personFilter);
}
