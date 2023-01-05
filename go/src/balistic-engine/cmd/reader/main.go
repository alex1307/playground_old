package main

import (
	"balistic-engine/pkg/config"
	"bufio"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"sync"
	"time"

	"go.uber.org/zap"
)

var FILE = "/Users/ayagasha/data/indiv18/proto.txt"

func main() {
	config.Setup()
	var wg sync.WaitGroup
	wg.Add(4)
	go StartReader(FILE, &wg)
	go StartReader(FILE, &wg)
	go StartReader(FILE, &wg)
	go StartReader(FILE, &wg)
	wg.Wait()
}

func StartReader(fileName string, wg *sync.WaitGroup) {
	defer wg.Done()
	start := time.Now()
	var file *os.File
	var err error
	var stat os.FileInfo
	for {
		if stat, err = os.Stat(fileName); errors.Is(err, fs.ErrNotExist) {
			config.AppLogger.Info("Can't find a file...", zap.Error(err))
			time.Sleep(500 * time.Millisecond)
			continue
		} else {
			config.AppLogger.Info("Found",
				zap.String("file name", fileName),
				zap.Int64("file size", stat.Size()))
			if stat.Size() == 0 {
				config.AppLogger.Info("File is empty. Waiting...")
				time.Sleep(500 * time.Millisecond)
				continue
			}
			file, _ = os.OpenFile(fileName, os.O_RDONLY|os.O_SYNC, 0666)
			break
		}
	}

	n, _ := ReadFromFile(file)
	config.AppLogger.Info("Source file. All records: ", zap.Int("count", n))
	config.AppLogger.Info("Total time: ", zap.Duration("elapsed time", time.Since(start)))
}

func ReadFromFile(File *os.File) (int, error) {
	scanner := bufio.NewReader(File)
	linesPool := sync.Pool{New: func() interface{} {
		lines := make([]byte, 256*1024)
		return lines
	}}
	read_bytes := 0
	for {
		buffer := linesPool.Get().([]byte)
		n, err := scanner.Read(buffer)
		buffer = buffer[:n]
		read_bytes += n
		if n == 0 {
			if err != nil {
				fmt.Println(err)
				break
			}
			if err == io.EOF {
				break
			}
			return read_bytes, err
		}
		nextUntillNewline, err := scanner.ReadBytes('\r')

		if err != io.EOF {
			buffer = append(buffer, nextUntillNewline...)
		}
		read_bytes += len(nextUntillNewline)

		linesPool.Put(buffer)
	}
	return read_bytes, nil
}
