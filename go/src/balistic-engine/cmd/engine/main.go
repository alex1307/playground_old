package main

import (
	"balistic-engine/pkg/math"
	"encoding/json"

	"go.uber.org/zap"
)

func main() {
	logger := Setup()
	logger.Info("Hello from visual code... This is the working engine")
	velocity := math.Coordinates{X: 8.0, Y: 0.0, T: 0.0}
	logger.Info("Y velocity: ", zap.Float64("Velocity_X", velocity.X),
		zap.Float64("Velocity_Y", velocity.Y), zap.Float64("Velocity_Y", velocity.X))
}

func Setup() *zap.Logger {
	rawJSON := []byte(`{
        "level": "debug",
        "encoding": "json",
        "outputPaths": ["stdout"],
        "errorOutputPaths": ["stderr"],
        "encoderConfig": {
          "messageKey": "message",
          "levelKey": "level",
          "levelEncoder": "lowercase"
        }
      }`)
	var cfg zap.Config
	if err := json.Unmarshal(rawJSON, &cfg); err != nil {
		panic(err)
	}
	logger, err := cfg.Build()
	if err != nil {
		panic(err)
	}
	defer logger.Sync()
	return logger
}

/*
func main(){
	if err := cmd.Execute(); err != nil {
		log.Fatal(err)
		os.Exit(1)
	}
}
*/
