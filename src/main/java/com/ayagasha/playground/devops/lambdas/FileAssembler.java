package com.ayagasha.playground.devops.lambdas;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

@Slf4j
public class FileAssembler {

    final private ExecutorService workers = Executors.newFixedThreadPool(4);
    private final ByteUnit unit;
    private final Path filePath;
    private final long unitBlocks;
    private final String dir;
    private final RandomAccessFile targetFile;
    private FileChannel targetChannel;


    public FileAssembler(String directory, String fileName, long unitBlocks, ByteUnit unit) throws IOException {
        this.unit = unit;
        this.unitBlocks = unitBlocks;
        filePath = Paths.get(fileName);
        dir = directory;
        targetFile = new RandomAccessFile(fileName, "rw");
        targetChannel = targetFile.getChannel();
    }

    private boolean appendToTargetFile(final Path filePart) throws IOException {

        if (Files.exists(filePart)) {

            final long fileSize = Files.size(filePart);
            final String index = filePart.toFile().getName().split(".part")[0];
            try (
                    RandomAccessFile source = new RandomAccessFile(filePart.toAbsolutePath().toString(), "r");
                    FileChannel sourceChannel = source.getChannel()
            ) {
                int fileIndex = Integer.valueOf(index);
                targetChannel.position(fileIndex * unit.getUnit() * unitBlocks);
                long read = sourceChannel.transferTo(0, fileSize, targetChannel);

                log.info("Successfully copied {} bytes from {} to file {} starting at position {}. Target file size: {}, Read bytes {}",
                        fileSize, filePart.toAbsolutePath(), filePath.toAbsolutePath(), targetChannel.position(), targetFile.length(), read);
                return true;
            } catch (IOException e) {
                log.error("Failed to append {} to target file {}", filePart.toAbsolutePath(), filePath.toAbsolutePath());
            }

            return false;
        }

        throw new IOException(String.format("File %s does not exist", filePart.toAbsolutePath()));
    }

    public Future<Boolean> execute(Path entry) {
        return workers.submit(() -> appendToTargetFile(entry));
    }


    public Boolean execute() throws IOException{
        final List<Boolean> statusList = new ArrayList<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(Paths.get(dir))) {

            for (Path entry: stream) {
                if(entry.toFile().getName().indexOf(".part") > 0) {
                    Future<Boolean> result = execute(entry);
                    Boolean completed = result.get(5, TimeUnit.SECONDS);
                    statusList.add(completed);
                }

            }

        } catch (Exception ex) {
            targetChannel.close();
            targetFile.close();
            log.error("Failed to assemble file: {}", filePath.toAbsolutePath(), ex);
        }

        return statusList
                .stream()
                .allMatch(e -> e);
    }

}
