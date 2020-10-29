package com.ayagasha.parking.datacore.configuration;

import com.ayagasha.parking.datacore.model.Reservation;
import com.ayagasha.parking.datacore.repository.ReservationRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;
import org.springframework.data.r2dbc.repository.config.EnableR2dbcRepositories;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;

@RequiredArgsConstructor
@Component
@Slf4j
@EnableR2dbcRepositories
public class DataInitializer {

    private static Long id = 100L;
    private final ReservationRepository reservationRepository;

    @EventListener(ApplicationReadyEvent.class)
    public void ready() {
        Flux<Reservation> reservations = Flux
                .just("Rohit",
                        "Mohit",
                        "Alex",
                        "Jarrar",
                        "Oliver",
                        "Rizwan",
                        "Jon",
                        "Josh",
                        "The Rock")
                .map(n -> Reservation.builder().name(n).build())
                .flatMap(reservationRepository::save);

        reservationRepository
                .deleteAll()
                .thenMany(reservations)
                .thenMany(reservationRepository.findAll())
                .subscribe(System.out::println);

    }

    private Reservation setId(@NonNull Reservation reservation) {
        reservation.setId(id++);
        return reservation;
    }

}
