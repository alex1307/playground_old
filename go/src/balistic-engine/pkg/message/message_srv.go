package message

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"time"

	"go.uber.org/zap"
)

var MAX_RETRIES int = 3
var MAX_FAILURES int = 10

type Payload interface {
	GetID() string
	GetTeamID() string
	GetCoordinates() math.Coordinates
}

type Message interface {
	GetID() string
	From() string
	To() []string
	Content() Payload
}

type Server struct {
	subscribers map[string]chan Payload
	cached      map[string]Payload
	failures    map[string]int
}

func NewServer() *Server {
	return &Server{
		subscribers: make(map[string]chan Payload, 10),
		cached:      make(map[string]Payload, 100),
		failures:    make(map[string]int, 10),
	}
}

func (s *Server) Subscribe(name string, sub chan Payload) string {
	if _, ok := s.subscribers[name]; !ok {
		s.subscribers[name] = sub
		return name
	}
	return ""
}

func (s *Server) retry(key string, ch chan Payload, message Payload) {
	for i := 0; i < MAX_RETRIES; i++ {
		select {
		case ch <- message:
			return
		case <-time.After(100 * time.Millisecond):
			continue
		}
	}
	config.AppLogger.Error("Message has been dropped",
		zap.String("topic", key),
		zap.String("message_uuid", message.GetID()))
	s.failures[key] = s.failures[key] + 1
	if s.failures[key] > MAX_FAILURES {
		s.delete_subscriber(key, message)
	}
}

func (s *Server) delete_subscriber(subscriber string, message Payload) {
	s.cached[message.GetID()] = message
	delete(s.subscribers, subscriber)
	close(s.subscribers[subscriber])
	delete(s.subscribers, subscriber)
}

func (s *Server) sendToAll(msg Payload) {
	for k, ch := range s.subscribers {
		go func(k string, ch chan Payload) {
			select {
			case ch <- msg:
			default: // drop message if channel is full
				s.retry(k, ch, msg)
			}
		}(k, ch)
	}
}

func (s *Server) Send(msg Message) {
	if msg.To() == nil || len(msg.To()) == 0 {
		s.sendToAll(msg.Content())
		return
	}

	for _, reciever := range msg.To() {
		ch, ok := s.subscribers[reciever]
		if ok {
			go func(ch chan Payload) {
				select {
				case ch <- msg.Content():

				default: // drop message if channel is full
					s.retry(reciever, ch, msg.Content())
				}
			}(ch)
		}
	}
}

func (s *Server) Close() {
	topics := make([]string, len(s.subscribers))
	for k, ch := range s.subscribers {
		close(ch)
		topics = append(topics, k)
	}
	for _, topic := range topics {
		delete(s.subscribers, topic)
	}
}
