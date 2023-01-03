package test

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/launcher"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/message"
	"balistic-engine/pkg/projectile"
)

var COORDINATES = math.Coordinates{X: 0, Y: 0, T: 0}
var RADIANS_30, _ = math.DegreesToRadians(30)
var RADIANS_35, _ = math.DegreesToRadians(35)
var RADIANS_40, _ = math.DegreesToRadians(40)
var RADIANS_45, _ = math.DegreesToRadians(45)
var RADIANS_50, _ = math.DegreesToRadians(50)
var RADIANS_55, _ = math.DegreesToRadians(55)
var RADIANS_60, _ = math.DegreesToRadians(60)
var RADIANS_65, _ = math.DegreesToRadians(65)
var BROKER *message.Server = message.NewServer()
var LAUNCHER *launcher.Launcher = launcher.NewLauncher(config.TEAM_UA, BROKER, COORDINATES)
var LAUNCHER2 *launcher.Launcher = launcher.NewLauncher(config.TEAM_RS, BROKER, math.Coordinates{X: 300, Y: 0, T: 0})
var PROJECTILES *projectile.ProjectileHandler = projectile.NewProjectile(BROKER)

func Setup() {
	config.Setup()
	launcher_config_1 := []launcher.ShootingConfiguration{
		{
			Velocity: 55.0,
			Radians:  RADIANS_30,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_35,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_40,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_45,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_50,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_55,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_60,
		}}
	launcher_config_2 := []launcher.ShootingConfiguration{{
		Velocity: -55.0,
		Radians:  RADIANS_30,
	},
		{
			Velocity: -55.0,
			Radians:  RADIANS_35,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_40,
		},
		{
			Velocity: -55.0,
			Radians:  RADIANS_45,
		},
		{
			Velocity: 55.0,
			Radians:  RADIANS_50,
		},
		{
			Velocity: -55.0,
			Radians:  RADIANS_55,
		},
		{
			Velocity: -55.0,
			Radians:  RADIANS_60,
		}}
	LAUNCHER.ConfigureShooting(launcher_config_1)
	LAUNCHER2.ConfigureShooting(launcher_config_2)

}
