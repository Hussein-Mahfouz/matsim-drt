package com.husseinmahfouz.matsim.drt.waiting;

import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.drt.run.DrtConfigGroup;
import org.matsim.core.config.Config;
import org.matsim.core.controler.events.IterationEndsEvent;
import org.matsim.core.controler.listener.IterationEndsListener;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Singleton
public class DrtWaitingTimeProvider implements IterationEndsListener {

    private static final Logger log = LogManager.getLogger(DrtWaitingTimeProvider.class);

    // Map: mode -> (timebin -> average wait time in seconds)
    private final Map<String, Map<Integer, Double>> waitingTimesByModeAndTime = new HashMap<>();
    private final String outputDirectory;
    private final Set<String> drtModes;
    private final Map<String, Double> defaultWaitTimesByMode = new HashMap<>();

    @Inject
    public DrtWaitingTimeProvider(Config config) {
        this.outputDirectory = config.controller().getOutputDirectory();

        // Auto-detect DRT modes from config
        MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(config);
        this.drtModes = multiModeDrtConfig.getModalElements().stream().map(DrtConfigGroup::getMode)
                .collect(Collectors.toSet());

        // Initialize with mode-specific maxWaitTime from config
        for (DrtConfigGroup drtConfig : multiModeDrtConfig.getModalElements()) {
            String mode = drtConfig.getMode();

            // Get (or create) the wrapper and the default constraints set
            var constraintsParams = drtConfig.addOrGetDrtOptimizationConstraintsParams();
            var defaultConstraints =
                    constraintsParams.addOrGetDefaultDrtOptimizationConstraintsSet();

            double maxWaitTime = defaultConstraints.maxWaitTime;

            // Handle NaN or infinite case (if maxWaitTime not set in config)
            if (Double.isNaN(maxWaitTime) || !Double.isFinite(maxWaitTime)) {
                maxWaitTime = 600.0; // Default 10 min
                log.warn("⚠️ maxWaitTime not set for mode {}. Using default: {} seconds", mode,
                        maxWaitTime);

            }

            defaultWaitTimesByMode.put(mode, maxWaitTime);
            waitingTimesByModeAndTime.put(mode, new HashMap<>());

            log.info("DrtWaitingTimeProvider: Mode {} initialized with maxWaitTime: {} seconds",
                    mode, maxWaitTime);

        }
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

                // Calculate statistics for logging
                int totalBins = timeBinnedWaits.size();
                int validBins = (int) timeBinnedWaits.values().stream().filter(v -> v > 0).count();
                double avgWaitTime = timeBinnedWaits.values().stream().filter(v -> v > 0) // Only
                                                                                          // include
                                                                                          // non-zero
                                                                                          // values
                        .mapToDouble(Double::doubleValue).average()
                        .orElse(defaultWaitTimesByMode.get(mode));

                log.info(
                        "✓ Loaded wait times for {} from iteration {} (avg: {} sec across {}/{} valid time bins)",
                        mode, iteration, String.format("%.1f", avgWaitTime), validBins, totalBins);


            } catch (Exception e) {
                log.warn("⚠️ Could not read wait stats for {}: {}. Using default maxWaitTime.",
                        mode, e.getMessage());

            }
        }
    }

    private Map<Integer, Double> parseWaitStats(String filepath) throws Exception {
        Map<Integer, Double> timeBinnedWaits = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filepath))) {
            String header = reader.readLine(); // Skip header
            String line;

            while ((line = reader.readLine()) != null) {
                String[] parts = line.split("\t"); // Assuming tab-separated values
                if (parts.length < 3)
                    continue; // Skip malformed lines

                String timebinStr = parts[0].trim(); // e.g., "08:00:00"
                int legs = Integer.parseInt(parts[1].trim());
                double avgWait = Double.parseDouble(parts[2].trim()); // "average_wait" column

                int timebinSeconds = parseTimeToSeconds(timebinStr);

                // ✅ Only store if there were actual requests (legs > 0 AND avgWait > 0)
                if (legs > 0 && avgWait > 0) {
                    timeBinnedWaits.put(timebinSeconds, avgWait);
                }
                // Otherwise, skip this bin (will use default when queried)
            }
        }

        return timeBinnedWaits;
    }

    private int parseTimeToSeconds(String timeStr) {
        String[] parts = timeStr.split(":");
        int hours = Integer.parseInt(parts[0]);
        int minutes = Integer.parseInt(parts[1]);
        int seconds = Integer.parseInt(parts[2]);
        return hours * 3600 + minutes * 60 + seconds;
    }

    /**
     * Get average waiting time for a DRT mode at a specific time of day
     */
    public double getWaitingTime(String mode, double departureTime) {
        Map<Integer, Double> timeBinnedWaits = waitingTimesByModeAndTime.get(mode);

        // Case 1: No data loaded yet OR no valid time bins
        if (timeBinnedWaits == null || timeBinnedWaits.isEmpty()) {
            return defaultWaitTimesByMode.get(mode);
        }

        // Find the closest time bin with actual data
        int closestBin = findClosestTimebin(timeBinnedWaits, (int) departureTime);

        // Case 2: No valid bin found (shouldn't happen but safety check)
        if (closestBin == -1) {
            return defaultWaitTimesByMode.get(mode);
        }

        double waitTime = timeBinnedWaits.get(closestBin);

        // Case 3: Bin exists but has zero value (shouldn't happen after filtering, but safety
        // check)
        if (waitTime <= 0) {
            return defaultWaitTimesByMode.get(mode);
        }

        return waitTime;
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
