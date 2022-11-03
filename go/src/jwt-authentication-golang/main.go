package main

import (
	"jwt-authentication-golang/controllers"
	"jwt-authentication-golang/database"
	"jwt-authentication-golang/middlewares"

	"github.com/gin-gonic/gin"
)

func main() {
	// Initialize Database
	database.Connect("root:1234@tcp(localhost:3306)/DB_USER?parseTime=true")
	database.Migrate()

	// Initialize Router
	router := initRouter()
	router.RunTLS(":8080", "/Users/ayagasha/Software/cert/certificate.pem", "/Users/ayagasha/Software/cert/key.pem")
}

func initRouter() *gin.Engine {
	router := gin.Default()
	api := router.Group("/api")
	{
		api.POST("/token", controllers.GenerateToken)
		api.POST("/user/register", controllers.RegisterUser)
		secured := api.Group("/secured").Use(middlewares.Auth())
		{
			secured.GET("/ping", controllers.Ping)
		}
	}
	return router
}
