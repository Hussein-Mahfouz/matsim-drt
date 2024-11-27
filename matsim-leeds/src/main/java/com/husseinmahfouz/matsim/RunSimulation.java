package com.husseinmahfouz.matsim;

import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;

public class RunSimulation {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: RunSimulation <config-file>");
            System.exit(1);
        }

        String configFile = args[0];
        Config config = ConfigUtils.loadConfig(configFile);
        Scenario scenario = ScenarioUtils.loadScenario(config);

        Controler controler = new Controler(scenario);
        // controler.addOverridingModule(new IDFModeChoiceModule());
        // controler.addOverridingModule(new PolicyExtension());

        controler.run();
    }
}
