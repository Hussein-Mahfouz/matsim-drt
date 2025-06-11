package com.husseinmahfouz.matsim.sample;

import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PopulationWriter;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.gbl.MatsimRandom;
import org.matsim.core.population.io.PopulationReader;
import org.matsim.core.scenario.MutableScenario;
import org.matsim.core.scenario.ScenarioUtils;

import java.util.HashMap;
import java.util.Map;

public class CreatePopulationSample {

    public static void main(String[] args) {
        Map<String, String> argMap = parseArgs(args);

        if (!argMap.containsKey("input") || !argMap.containsKey("output") || !argMap.containsKey("sample")) {
            System.out.println(
                "Usage: CreatePopulationSample --input <input-population-file> --output <output-population-file> --sample <sample-size>");
            return;
        }

        MutableScenario sc = ScenarioUtils.createMutableScenario(ConfigUtils.createConfig());

        PopulationReader reader = new PopulationReader(sc);

        double sample = Double.parseDouble(argMap.get("sample"));

        // input population file
        reader.readFile(argMap.get("input"));

        MutableScenario sc2 = ScenarioUtils.createMutableScenario(ConfigUtils.createConfig());

        for (Person person : sc.getPopulation().getPersons().values()) {
            if (MatsimRandom.getRandom().nextDouble() < sample)
                sc2.getPopulation().addPerson(person);
        }

        // output population file
        new PopulationWriter(sc2.getPopulation()).write(argMap.get("output"));
    }

    private static Map<String, String> parseArgs(String[] args) {
        Map<String, String> argMap = new HashMap<>();
        for (int i = 0; i < args.length; i += 2) {
            if (i + 1 < args.length) {
                argMap.put(args[i].replace("--", ""), args[i + 1]);
            }
        }
        return argMap;
    }
}