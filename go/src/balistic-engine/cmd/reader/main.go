package main

import (
	"balistic-engine/grpc/reader"
	"balistic-engine/pkg/config"
	"bufio"
	"bytes"
	"encoding/binary"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"go.uber.org/zap"
	"google.golang.org/protobuf/proto"
)

type entry struct {
	FirstName string
	LastName  string
	Date      int
}

var FILE = "/Users/ayagasha/data/indiv18/proto.txt"

func main() {
	config.Setup()
	start := time.Now()
	file, err := os.Open(os.Args[1])
	os.Remove(FILE)
	counter, _ := ReadProtoFromFile(os.Args[1])
	config.AppLogger.Info("Source file. All records: ", zap.Int("count", counter))
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	commonName := ""
	commonCount := 0
	scanner := bufio.NewScanner(file)
	nameMap := make(map[string]int)
	dateMap := make(map[int]int)

	namesCounted := false
	namesCount := 0
	fileLineCount := int64(0)

	linesChunkLen := 128 * 1024
	linesChunkPoolAllocated := int64(0)
	linesPool := sync.Pool{New: func() interface{} {
		lines := make([]string, 0, linesChunkLen)
		atomic.AddInt64(&linesChunkPoolAllocated, 1)
		return lines
	}}
	lines := linesPool.Get().([]string)[:0]

	entriesPoolAllocated := int64(0)
	entriesPool := sync.Pool{New: func() interface{} {
		entries := make([]entry, 0, linesChunkLen)
		atomic.AddInt64(&entriesPoolAllocated, 1)
		return entries
	}}

	mutex := &sync.Mutex{}
	wg := sync.WaitGroup{}
	scanner.Scan()
	for {
		lines = append(lines, scanner.Text())
		willScan := scanner.Scan()

		if len(lines) == linesChunkLen || !willScan {
			linesToProcess := lines
			wg.Add(len(linesToProcess))
			go processLines(lines, nameMap, dateMap, &commonName, &commonCount, &namesCounted, &namesCount, &fileLineCount, mutex, &wg, &linesPool, &entriesPool)
			lines = linesPool.Get().([]string)[:0]
		}
		if !willScan {
			break
		}
	}
	wg.Wait()

	// report c2: names at index
	config.AppLogger.Info("Timer: ", zap.Duration("elapsed time", time.Since(start)))

	// // report c1: total number of lines
	config.AppLogger.Info("Total file line count: ", zap.Int64("count", fileLineCount))
	config.AppLogger.Info("Line count time: ", zap.Duration("time", time.Since(start)))

	// // report c3: donation frequency
	all := 0
	for k, v := range dateMap {
		config.AppLogger.Info("Donations per month and year: ", zap.Int("date", k), zap.Int("count", v))
		all += v
	}
	config.AppLogger.Info("Total donations: ", zap.Int("count", all))
	config.AppLogger.Info("Donations: ", zap.Duration("time", time.Since(start)))

	// // report c4: most common firstName
	config.AppLogger.Info("Most common name: ",
		zap.String("name", commonName), zap.Int("count", commonCount))
	config.AppLogger.Info("Most common name: ", zap.Duration("time", time.Since(start)))
	counter, _ = ReadProtoFromFile(FILE)
	config.AppLogger.Info("Proto count: ", zap.Int("count", counter))
	config.AppLogger.Info("Time to complete everything: ", zap.Duration("time", time.Since(start)))
}

func processLines(lines []string, nameMap map[string]int, dateMap map[int]int, commonName *string, commonCount *int, namesCounted *bool, namesCount *int, fileLineCount *int64, mutex *sync.Mutex, wg *sync.WaitGroup, linesPool *sync.Pool, entriesPool *sync.Pool) {
	atomic.AddInt64(fileLineCount, int64(len(lines)))
	entries := entriesPool.Get().([]entry)[:0]
	for _, text := range lines {
		// get all the names
		entry := entry{}
		split := strings.SplitN(text, "|", 9)
		entry.LastName = strings.TrimSpace(split[7])

		// extract first names
		if entry.LastName != "" {
			startOfName := strings.Index(entry.LastName, ", ") + 2
			if endOfName := strings.Index(entry.LastName[startOfName:], " "); endOfName < 0 {
				entry.FirstName = entry.LastName[startOfName:]
			} else {
				entry.FirstName = entry.LastName[startOfName : startOfName+endOfName]
			}
			if cs := strings.Index(entry.FirstName, ","); cs > 0 {
				entry.FirstName = entry.FirstName[:cs]
			}
		}
		// extract dates
		entry.Date, _ = strconv.Atoi(split[4][:6])
		entries = append(entries, entry)
	}
	linesPool.Put(lines)
	entriesPool.Put(entries)
	if len(entries) != len(lines) {
		fmt.Println("!!!!!! entries: ", len(entries), "lines: ", len(lines))
	}
	outFile, err := os.OpenFile("/Users/ayagasha/data/indiv18/proto.txt", os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0666)
	if err != nil {
		return
	}
	mutex.Lock()
	WriteProtoToFile(outFile, *entriesPool)
	newFunction(entries, nameMap, commonCount, commonName, namesCounted, namesCount, dateMap)
	mutex.Unlock()
	wg.Add(-len(entries))
}

func newFunction(entries []entry, nameMap map[string]int, commonCount *int, commonName *string, namesCounted *bool, namesCount *int, dateMap map[int]int) {

	for _, entry := range entries {
		if len(entry.FirstName) != 0 {
			nameCount := nameMap[entry.FirstName] + 1
			nameMap[entry.FirstName] = nameCount
			if nameCount > *commonCount {
				*commonCount = nameCount
				*commonName = entry.FirstName
			}
		}
		if *namesCounted {
			if *namesCount == 0 {
				config.AppLogger.Info("Name: ", zap.String("name", entry.LastName), zap.Int("index", 0))
			} else if *namesCount == 432 {
				config.AppLogger.Info("Name: ", zap.String("name", entry.LastName), zap.Int("index", 432))
			} else if *namesCount == 43243 {
				config.AppLogger.Info("Name: ", zap.String("name", entry.LastName), zap.Int("index", 43243))
				*namesCounted = true
			}
			*namesCount++
		}
		dateMap[entry.Date]++
	}

}

func WriteProtoToFile(outFile *os.File, pool sync.Pool) error {
	buffer := bytes.NewBuffer(make([]byte, 0, 128*1024))
	for _, item := range pool.Get().([]entry) {

		record := &reader.Record{
			FirstName: item.FirstName,
			LastName:  item.LastName,
			Date:      int32(item.Date),
		}
		encodedData, err := proto.Marshal(record)
		if err != nil {
			return err
		}

		// writing the length of the encoded item before writing the data
		buf := make([]byte, 4)
		binary.LittleEndian.PutUint32(buf, uint32(len(encodedData)))
		buffer.Write(buf)
		buffer.Write(encodedData)

		if buffer.Len() >= 128*1024 {
			if _, err := outFile.Write(buffer.Bytes()); err != nil {
				return err
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

func ReadProtoFromFile(filepath string) (int, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return -1, err
	}

	linesChunkLen := 128 * 1024
	linesChunkPoolAllocated := int64(0)
	linesPool := sync.Pool{New: func() interface{} {
		lines := make([]string, 0, linesChunkLen)
		atomic.AddInt64(&linesChunkPoolAllocated, 1)
		return lines
	}}
	lines := linesPool.Get().([]string)[:0]

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	counter := 0
	for {
		lines = append(lines, scanner.Text())
		willScan := scanner.Scan()

		if len(lines) == linesChunkLen || !willScan {
			linesToProcess := lines
			lines = linesPool.Get().([]string)[:0]
			counter += len(linesToProcess)
		}
		if !willScan {
			break
		}

	}
	return counter, nil
}
