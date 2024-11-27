package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.standalone_mode_choice.StandaloneModeChoiceConfigurator;
import com.husseinmahfouz.matsim.dmc.mode_choice.LeedsModeChoiceModule;
import org.matsim.core.config.CommandLine;
import org.matsim.core.config.Config;
import org.matsim.core.controler.AbstractModule;

import java.util.List;

public class LeedsStandaloneModeChoiceConfigurator extends StandaloneModeChoiceConfigurator {
    public LeedsStandaloneModeChoiceConfigurator(Config config, CommandLine commandLine) {
        super(config, commandLine);
    }

    protected List<AbstractModule> getSpecificModeChoiceModules() {
        return List.of(new LeedsModeChoiceModule(this.getCommandLine()));
    }
}
