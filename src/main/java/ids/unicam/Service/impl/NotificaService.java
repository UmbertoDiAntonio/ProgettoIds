package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.NotificaRepository;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class NotificaService {

    private final NotificaRepository repository;

    public NotificaService(NotificaRepository repository) {
        this.repository = repository;
    }


    public void creaNotificaApprovazione(@NotNull Curatore curatore, PuntoInteresse puntoInteresse, @NotNull Stato stato) {
        repository.save(new NotificaBuilder().withTitolo("Valutazione: " + puntoInteresse.getNome())
                .withDescrizione(curatore.getUsername() + " " + (Boolean.TRUE.equals(stato.asBoolean()) ? "ha approvato " : "non ha approvato ") +
                        "\"" + puntoInteresse.getNome() + "\"")
                .withDestinatario(puntoInteresse.getCreatore()).build());
    }


    public void creaNotificaApprovazione(Curatore curatore, MaterialeGenerico materialeGenerico, Stato stato) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Valutazione del materiale creato da: " + materialeGenerico.getCreatore().getUsername())
                        .withDescrizione(curatore.getUsername() + " " + (Boolean.TRUE.equals(stato.asBoolean()) ? "ha approvato " : "non ha approvato ") +
                                "il materiale con id " + materialeGenerico.getId())
                        .withDestinatario(materialeGenerico.getCreatore()).build());
    }

    public void creaNotificaVittoriaContest(Animatore animatore, Contest contest, MaterialeGenerico vincitore) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Vittoria Contest: " + vincitore.getCreatore().getUsername())
                        .withDescrizione("Hai vinto il contest " + contest.getNomeContest() + " creato da " + animatore.getUsername() + " con il materiale " + vincitore.getId())
                        .withDestinatario(vincitore.getCreatore()).build());
    }

    public void creaNotificaTermineContest(Contest contest, TuristaAutenticato destinatario) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Contest: " + contest.getNomeContest())
                        .withDescrizione("il contest " + contest.getNomeContest() + " creato da " + contest.getCreatore().getUsername() + (contest.getMaterialeVincitore() != null ? " è terminato " : (" terminerà il " + contest.getExpireDate())))
                        .withDestinatario(destinatario).build());
    }

    public void creaNotificaInvitoContest(Animatore animatore, Contest contest, TuristaAutenticato destinatario) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Contest: " + contest.getNomeContest())
                        .withDescrizione("Sei stato invitato a unirti al contest " + contest.getNomeContest() + " da parte dell'animatore " + animatore.getUsername())
                        .withDestinatario(destinatario).build());
    }

    public void creaNotificaIngressoContest(Contest contest, TuristaAutenticato turista) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Contest: " + contest.getNomeContest())
                        .withDescrizione(turista.getUsername() + " si è unito al Contest: " + contest.getNomeContest())
                        .withDestinatario(contest.getCreatore()).build());
    }


    public List<Notifica> getNotifiche(TuristaAutenticato turistaAutenticato) {
        return repository.findByUsernameDestinatario(turistaAutenticato.getUsername());
    }


    public Notifica save(Notifica build) {
        return repository.save(build);
    }

    @Transactional
    public void rimuoviNotificheByUsername(String usernameTurista) {
        repository.deleteByUsernameDestinatario(usernameTurista);
    }


}
