package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.NotificaRepository;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.attori.Turista;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;

import java.util.List;

import static ids.unicam.Main.logger;

@Service
public class NotificaServiceImpl {

    private final NotificaRepository notificaRepository;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;


    public NotificaServiceImpl(NotificaRepository notificaRepository, TuristaAutenticatoServiceImpl turistaAutenticatoService) {
        this.notificaRepository = notificaRepository;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }

    public Notifica creaNotifica(@NotNull Curatore curatore, PuntoInteresse puntoInteresse, @NotNull Stato stato) {
        if (turistaAutenticatoService.findById(puntoInteresse.getCreatore().getUsername()).isEmpty()) {
            logger.warn("il creatore non esiste piu'.");
            return null;
        }
        return notificaRepository.save(new NotificaBuilder().withTitolo("Valutazione: " + puntoInteresse.getNome())
                .withDescrizione(curatore.getUsername() + " " + (Boolean.TRUE.equals(stato.asBoolean()) ? "ha approvato " : "non ha approvato ") +
                        "\"" + puntoInteresse.getNome()+"\"")
                .withDestinatario(puntoInteresse.getCreatore()).build());
    }


    public Notifica creaNotifica(Curatore curatore, MaterialeGenerico materialeGenerico, Stato stato) {

        return notificaRepository.save(
                new NotificaBuilder()
                        .withTitolo("Valutazione del materiale creato da: " + materialeGenerico.getCreatore().getUsername())
                        .withDescrizione(curatore.getUsername() + " " + (Boolean.TRUE.equals(stato.asBoolean()) ? "ha approvato " : "non ha approvato ") +
                                "il materiale con id " + materialeGenerico.getId())
                        .withDestinatario(materialeGenerico.getCreatore()).build());
    }

    public List<Notifica> getNotifiche(TuristaAutenticato turistaAutenticato) {
        return notificaRepository.findByUsernameDestinatario(turistaAutenticato.getUsername());
    }


    public Notifica save(Notifica build) {
        return notificaRepository.save(build);
    }
}
