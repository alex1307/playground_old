package main

import (
	"balistic-engine/grpc/reader"
	"balistic-engine/pkg/config"
	"bytes"
	"io"
	"io/fs"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"go.uber.org/zap"
	"google.golang.org/protobuf/proto"
)

var FILE string = "/Users/ayagasha/data/indiv18/proto.txt"
var FILE_TEMPLATE string = "/Users/ayagasha/data/indiv18/proto_#.txt"
var counter int = 0
var maximum_files = 10
var maximum_size int64 = 256 * 1024 * 1024
var NUMBER_OF_LINES = 10_000_000
var permissions = os.O_APPEND | os.O_CREATE | os.O_WRONLY | os.O_SYNC
var file_mode = 0666
var buffer_size = 1024 * 1024 * 16

func main() {
	config.Setup()
	truncate()
	start := time.Now()
	records := make([]*reader.Record, 0, NUMBER_OF_LINES)
	for i := 0; i < NUMBER_OF_LINES; i++ {
		records = append(records, &reader.Record{
			FirstName: "John",
			LastName:  "Doe",
			Date:      int32(32 + i%95),
		})
	}
	config.AppLogger.Info("Records generated in: ",
		zap.Duration("elapsed time", time.Since(start)),
		zap.Int("records count", len(records)))

	outFile, err := RollingFile(nil)
	if err != nil {
		config.AppLogger.Fatal("Error opening file", zap.Error(err))
	}

	for i := 0; i < 100; i++ {
		var wg sync.WaitGroup
		index := NUMBER_OF_LINES / 8
		for i := 0; i < 8; i++ {
			wg.Add(1)
			go func(i int) {
				WriteRecordsToFile(outFile, records[i*index:(i+1)*index], &wg)
			}(i)
		}
		wg.Wait()
		outFile, _ = RollingFile(outFile)
	}

	outFile.Close()

	config.AppLogger.Info("Data is saved into file in: ",
		zap.Duration("elapsed time", time.Since(start)),
		zap.String("file", outFile.Name()),
		zap.Int("records count", len(records)))
}

func WriteRecordsToFile(outFile *os.File, records []*reader.Record, wg *sync.WaitGroup) error {
	defer wg.Done()
	buffer := bytes.NewBuffer(make([]byte, 0, buffer_size))
	for i, item := range records {
		encodedData, err := proto.Marshal(item)
		if err != nil {
			return err
		}

		buffer.Write(encodedData)
		buffer.WriteByte('\r')
		if i%(NUMBER_OF_LINES/100) == 0 {
			outFile.Sync()
		}

		if buffer.Len() > buffer_size-512 {
			if _, err := outFile.Write(buffer.Bytes()); err != nil {
				return err
			} else {
				outFile.Sync()
			}
			buffer.Reset()
		}
	}
	if buffer.Len() > 0 {
		if _, err := outFile.Write(buffer.Bytes()); err != nil {
			return err
		}
		buffer.Reset()
	}
	return nil
}

func Archive(source *os.File, destination *os.File) error {
	if source == nil {
		return nil
	}
	if destination == nil {
		return nil
	}

	destination.Truncate(0)

	_, err := source.Seek(0, 0)
	if err != nil {
		return err
	}
	_, err = destination.Seek(0, 0)
	if err != nil {
		return err
	}
	_, err = io.Copy(destination, source)
	if err != nil {
		return err
	}
	return nil
}

func RollingFile(file *os.File) (*os.File, error) {
	if file == nil {
		rolling_file_name := strings.Replace(FILE_TEMPLATE, "#", strconv.Itoa(counter), 1)
		new_file, err := os.OpenFile(rolling_file_name, permissions, fs.FileMode(file_mode))
		if err != nil {
			return nil, err
		}
		return new_file, nil
	}
	stat, err := os.Stat(file.Name())
	if err != nil {
		return nil, err
	}
	if stat.Size() > maximum_size {
		err = file.Close()
		if err != nil {
			return nil, err
		}
		counter++
		rolling_file_name := strings.Replace(FILE_TEMPLATE, "#", strconv.Itoa(counter%maximum_files), 1)
		file, err = os.OpenFile(rolling_file_name, permissions, fs.FileMode(file_mode))
		if err != nil {
			return nil, err
		}
		file.Truncate(0)
	}
	return file, nil
}

func truncate() {
	for i := 0; i < maximum_files; i++ {
		rolling_file_name := strings.Replace(FILE_TEMPLATE, "#", strconv.Itoa(i), 1)
		if _, err := os.Stat(rolling_file_name); os.IsNotExist(err) {
			continue
		}
		os.Remove(rolling_file_name)
		config.AppLogger.Info("File has been removed",
			zap.String("file", rolling_file_name))
	}
}
