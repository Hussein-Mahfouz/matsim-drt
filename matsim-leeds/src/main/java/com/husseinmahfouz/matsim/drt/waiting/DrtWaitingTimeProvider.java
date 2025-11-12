package com.husseinmahfouz.matsim.drt.waiting;

import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.core.config.Config;
import org.matsim.core.controler.events.IterationEndsEvent;
import org.matsim.core.controler.listener.IterationEndsListener;
import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Singleton
public class DrtWaitingTimeProvider implements IterationEndsListener {

    // Map: mode -> (timebin -> average wait time in seconds)
    private final Map<String, Map<Integer, Double>> waitingTimesByModeAndTime = new HashMap<>();
    private final String outputDirectory;
    private final Set<String> drtModes;  // Auto-detected from config

    @Inject
    public DrtWaitingTimeProvider(Config config) {
        this.outputDirectory = config.controller().getOutputDirectory();
        
        // Auto-detect DRT modes from config
        MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(config);
        this.drtModes = multiModeDrtConfig.getModalElements().stream()
            .map(drtConfig -> drtConfig.getMode())
            .collect(Collectors.toSet());
        
        // Initialize with default values
        for (String mode : drtModes) {
            waitingTimesByModeAndTime.put(mode, new HashMap<>());
        }
        
        System.out.println("DrtWaitingTimeProvider initialized for modes: " + drtModes);
    }

    @Override
    public void notifyIterationEnds(IterationEndsEvent event) {
        int iteration = event.getIteration();

        // Read wait stats for each DRT mode
        for (String mode : drtModes) {
            String statsFile = outputDirectory + "/ITERS/it." + iteration + "/" + iteration
                    + ".waitStats_" + mode + ".csv";

            try {
                Map<Integer, Double> timeBinnedWaits = parseWaitStats(statsFile);
                waitingTimesByModeAndTime.put(mode, timeBinnedWaits);
                System.out.println(
                        "✓ Updated wait times for " + mode + " from iteration " + iteration);
            } catch (Exception e) {
                System.err.println(
                        "⚠️ Could not read wait stats for " + mode + ": " + e.getMessage());
            }
        }
    }

    private Map<Integer, Double> parseWaitStats(String filepath) throws Exception {
        Map<Integer, Double> timeBinnedWaits = new HashMap<>();

        BufferedReader reader = new BufferedReader(new FileReader(filepath));
        String header = reader.readLine(); // Skip header
        String line;

        while ((line = reader.readLine()) != null) {
            String[] parts = line.split(";");
            String timebinStr = parts[0]; // e.g., "08:00:00"
            double avgWait = Double.parseDouble(parts[2]); // "average_wait" column

            // Convert time string to seconds since midnight
            int timebinSeconds = parseTimeToSeconds(timebinStr);
            timeBinnedWaits.put(timebinSeconds, avgWait);
        }

        reader.close();
        return timeBinnedWaits;
    }

    private int parseTimeToSeconds(String timeStr) {
        // "08:00:00" -> 28800 seconds
        String[] parts = timeStr.split(":");
        int hours = Integer.parseInt(parts[0]);
        int minutes = Integer.parseInt(parts[1]);
        int seconds = Integer.parseInt(parts[2]);
        return hours * 3600 + minutes * 60 + seconds;
    }

    /**
     * Get average waiting time for a DRT mode at a specific time of day
     * 
     * @param mode DRT mode (e.g., "drtNW")
     * @param departureTime Time in seconds since midnight
     * @return Estimated wait time in seconds
     */
    public double getWaitingTime(String mode, double departureTime) {
        Map<Integer, Double> timeBinnedWaits = waitingTimesByModeAndTime.get(mode);

        if (timeBinnedWaits == null || timeBinnedWaits.isEmpty()) {
            return 300.0; // Default 5 minutes
        }

        // Find the closest time bin
        int closestBin = findClosestTimebin(timeBinnedWaits, (int) departureTime);
        return timeBinnedWaits.getOrDefault(closestBin, 300.0);
    }

    private int findClosestTimebin(Map<Integer, Double> timeBinnedWaits, int time) {
        int closestBin = -1;
        int minDiff = Integer.MAX_VALUE;

        for (int bin : timeBinnedWaits.keySet()) {
            int diff = Math.abs(bin - time);
            if (diff < minDiff) {
                minDiff = diff;
                closestBin = bin;
            }
        }

        return closestBin;
    }
}