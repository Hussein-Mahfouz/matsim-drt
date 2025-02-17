package com.husseinmahfouz.matsim.dmc;

import org.eqasim.core.scenario.RunInsertVehicles;
import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.scenario.ScenarioUtils;

// Used to create a vehicles file that has a vehicle with an id (all network modes) for each person
// in the
// population file. The vehicles file is then used to create a new population file with the vehicles
// assigned
// to each person. This is used specifically for eqasim emmisions calculations.
public class NetworkVehicleInserter {

    public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println(
                    "Usage: NetworkVehicleInserter <config-path> <input-population-path> <output-vehicles-path> <output-population-path>");
            return;
        }

        String configPath = args[0];
        String inputPopulationPath = args[1];
        String outputVehiclesPath = args[2];
        String outputPopulationPath = args[3];

        try {
            Config config = ConfigUtils.loadConfig(configPath);
            Scenario scenario = ScenarioUtils.createScenario(ConfigUtils.createConfig());
            new org.matsim.core.population.io.PopulationReader(scenario)
                    .readFile(inputPopulationPath);

            RunInsertVehicles.insertVehicles(config, scenario);

            new org.matsim.vehicles.MatsimVehicleWriter(scenario.getVehicles())
                    .writeFile(outputVehiclesPath);
            new org.matsim.core.population.io.PopulationWriter(scenario.getPopulation())
                    .write(outputPopulationPath);

            System.out.println("Vehicle insertion complete!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
