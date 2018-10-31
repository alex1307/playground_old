package com.ayagasha.playground.devops.lambdas;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Slf4j
public class FileSplitExecutor {
    final private ExecutorService workers = Executors.newFixedThreadPool(4);
    final private ExecutorService executor = Executors.newSingleThreadExecutor();

    final private ByteUnit unit;
    final private Long blockSize;
    final private BitSet parts;
    final private List<SplitFile> files;
    final private Path sourceFile;


    public FileSplitExecutor(String fileName, long unitBlocks, ByteUnit byteUnit) throws IOException {
        sourceFile = Paths.get(fileName);
        this.unit = byteUnit;
        this.blockSize = byteUnit.getUnit() * unitBlocks;
        Long fileSize = FileUtils.fileSize(fileName);
        Long countOfSplitFiles = fileSize / (unitBlocks * unit.getUnit());
        Long remaining = fileSize % (unitBlocks * unit.getUnit());
        parts = new BitSet(countOfSplitFiles.intValue() +
                remaining > 0 ? 1 : 0);
        files = new ArrayList<>(parts.size());

        final String targetDir = FileUtils.getDirectory(fileName) + "/" + UUID.randomUUID().toString();
        FileUtils.createDir(targetDir);
        for (int i = 0; i < countOfSplitFiles; i++) {
            files.add(
                    SplitFile
                            .builder()
                            .byteSize(unitBlocks * unit.getUnit())
                            .index(Long.valueOf(i))
                            .filePath(Paths.get(targetDir + "/" + i + ".part").toAbsolutePath().toString())
                            .build());
        }

        if (remaining > 0) {
            files.add(
                    SplitFile
                            .builder()
                            .byteSize(remaining)
                            .index(Long.valueOf(countOfSplitFiles))
                            .filePath(Paths.get(targetDir + "/" + countOfSplitFiles + ".part").toAbsolutePath().toString())
                            .build());
        }


    }


    public boolean execute(final SplitFile splitFile) {


        final Long position = splitFile.getIndex() * blockSize;
        try (RandomAccessFile targetFile = new RandomAccessFile(splitFile.getFilePath(), "rw");
             FileChannel targetChannel = targetFile.getChannel();
             RandomAccessFile source = new RandomAccessFile(sourceFile.toString(), "r");
             FileChannel sourceChannel = source.getChannel()
        ) {

            if(splitFile.getIndex() == 6) {
                log.info("Remaining....");
            }

            sourceChannel.position(position);
            targetChannel.transferFrom(sourceChannel, 0, splitFile.getByteSize());
            log.info("Transferred {} bytes from position {} to file {}",
                    splitFile.getByteSize(), position, splitFile.getFilePath());

        } catch (IOException e) {
            log.error("Can't transfer bytes from file {} at position {} to file {}",
                    sourceFile.toAbsolutePath(), position, splitFile.getFilePath());
            return false;
        }

        return true;
    }

    public boolean isTransferCompleted() {
        return parts.nextClearBit(0) != -1;
    }

    public List<SplitFile> getFiles() {
        return files;
    }

    public Future<Boolean> executeTask(SplitFile file) {
        return workers.submit(() -> execute(file));
    }

    public Boolean processFile() {

        List<Future<Boolean>> result =
                files
                        .stream()
                        .map(this::executeTask)
                        .collect(Collectors.toList());
        return result
                .stream()
                .allMatch(this::isSuccessful);
    }

    public Future<Boolean> execute() {
        return executor.submit(() -> processFile());
    }

    public Boolean isSuccessful(final Future<Boolean> future) {
        try {
            return future.get(5, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("Failed to read response {}");
            return false;
        }

    }
}
