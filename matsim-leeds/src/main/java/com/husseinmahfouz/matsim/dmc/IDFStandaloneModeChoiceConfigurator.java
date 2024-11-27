package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.standalone_mode_choice.StandaloneModeChoiceConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.IDFModeChoiceModule;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.Config;
import org.matsim.core.controler.AbstractModule;

import java.util.List;

public class IDFStandaloneModeChoiceConfigurator extends StandaloneModeChoiceConfigurator {
    public IDFStandaloneModeChoiceConfigurator(Config config, CommandLine commandLine) {
        super(config, commandLine);
    }

    protected List<AbstractModule> getSpecificModeChoiceModules() {
        return List.of(new IDFModeChoiceModule(this.getCommandLine()));
    }
}
