package ids.unicam.Service.impl;

import ids.unicam.models.contenuti.Notifica2;
import ids.unicam.DataBase.Repository.NotificaRepository;
import ids.unicam.models.Notifica;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.NotificaBuilder;
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
        if(turistaAutenticatoService.findById(puntoInteresse.getCreatore().getId()).isEmpty()) {
            logger.warn("il creatore non esiste piu'.");
            return null;
        }
        return notificaRepository.save(new Notifica("Valutazione: " + puntoInteresse.getNome(),
                curatore.getUsername() + " " + (stato.asBoolean() ? "ha approvato " : "non ha approvato ") +
                        "il " + puntoInteresse.getNome(), puntoInteresse.getCreatore()));
    }

    public Notifica creaNotifica(Curatore curatore, MaterialeGenerico materialeGenerico, Stato stato) {
        if(turistaAutenticatoService.findById(materialeGenerico.getCreatore().getId()).isEmpty()) {
            logger.warn("il creatore non esiste piu'.");
            return null;
        }
        return notificaRepository.save(new Notifica("Valutazione del materiale creato da: " + materialeGenerico.getCreatore().getUsername(),
                curatore.getUsername() + " " + (stato.asBoolean() ? "ha approvato " : "non ha approvato ") +
                        "il materiale con id " + materialeGenerico.getId(), materialeGenerico.getCreatore()));
    }

    public List<Notifica> getNotifiche(Contributor contributor) {
        return notificaRepository.findByRicevente(contributor);
    }

    public Notifica2 creaNotifica() {
        return new NotificaBuilder().withTitolo("titolo").withDescrizione("Desc").build();
    }

}
