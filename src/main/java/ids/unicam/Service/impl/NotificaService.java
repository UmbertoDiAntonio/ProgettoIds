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

    /**
     * Crea una notifica per il cambio di stato di un punto di interesse da parte di un curatore
     *
     * @param curatore       il curatore
     * @param puntoInteresse il punto di interesse
     * @param stato          lo stato impostato
     */
    public void creaNotificaApprovazione(@NotNull Curatore curatore, @NotNull PuntoInteresse puntoInteresse, @NotNull Stato stato) {
        repository.save(new NotificaBuilder().withTitolo("Valutazione: " + puntoInteresse.getNome())
                .withDescrizione(curatore.getUsername() + " " + (Boolean.TRUE.equals(stato.asBoolean()) ? "ha approvato " : "non ha approvato ") +
                        "\"" + puntoInteresse.getNome() + "\"")
                .withDestinatario(puntoInteresse.getCreatore()).build());
    }

    /**
     * Crea una notifica per il cambio di stato di un materiale da parte di un curatore
     *
     * @param curatore          il curatore
     * @param materialeGenerico il materiale
     * @param stato             lo stato impostato
     */
    public void creaNotificaApprovazione(@NotNull Curatore curatore, @NotNull MaterialeGenerico materialeGenerico, @NotNull Stato stato) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Valutazione del materiale creato da: " + materialeGenerico.getCreatore().getUsername())
                        .withDescrizione(curatore.getUsername() + " " + (Boolean.TRUE.equals(stato.asBoolean()) ? "ha approvato " : "non ha approvato ") +
                                "il materiale con id " + materialeGenerico.getId())
                        .withDestinatario(materialeGenerico.getCreatore()).build());
    }

    /**
     * Crea una notifica per la vittoria di un contest da parte di un animatore
     *
     * @param animatore l'animatore
     * @param contest   il contest vinto
     * @param vincitore il materiale che ha vinto
     */
    public void creaNotificaVittoriaContest(@NotNull Animatore animatore,@NotNull  Contest contest,@NotNull  MaterialeGenerico vincitore) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Vittoria Contest: " + vincitore.getCreatore().getUsername())
                        .withDescrizione("Hai vinto il contest " + contest.getNomeContest() + " creato da " + animatore.getUsername() + " con il materiale " + vincitore.getId())
                        .withDestinatario(vincitore.getCreatore()).build());
    }

    /**
     * Crea una notifica per la fine di un contest, per un partecipante
     *
     * @param contest      il contest
     * @param destinatario il partecipante
     */
    public void creaNotificaTermineContest(@NotNull Contest contest, @NotNull TuristaAutenticato destinatario) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Contest: " + contest.getNomeContest())
                        .withDescrizione("il contest " + contest.getNomeContest() + " creato da " + contest.getCreatore().getUsername() + (contest.getMaterialeVincitore() != null ? " è terminato " : (" terminerà il " + contest.getExpireDate())))
                        .withDestinatario(destinatario).build());
    }

    /**
     * Crea una notifica per l'invito a partecipare a un contest, da parte dell'animatore
     *
     * @param animatore    l'animatore
     * @param contest      il contest
     * @param destinatario l'invitato
     */
    public void creaNotificaInvitoContest(@NotNull Animatore animatore, @NotNull Contest contest, @NotNull TuristaAutenticato destinatario) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Contest: " + contest.getNomeContest())
                        .withDescrizione("Sei stato invitato a unirti al contest " + contest.getNomeContest() + " da parte dell'animatore " + animatore.getUsername())
                        .withDestinatario(destinatario).build());
    }

    /**
     * Crea una notifica per l'ingresso nel contest di un partecipante, al creatore del contest
     *
     * @param contest il contest
     * @param turista il nuovo partecipante
     */
    public void creaNotificaIngressoContest(@NotNull Contest contest, @NotNull TuristaAutenticato turista) {
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Contest: " + contest.getNomeContest())
                        .withDescrizione(turista.getUsername() + " si è unito al Contest: " + contest.getNomeContest())
                        .withDestinatario(contest.getCreatore()).build());
    }

    /**
     * Ottieni le notifiche di un utente
     *
     * @param turistaAutenticato l'utente che esegue l'operazione
     * @return la lista delle notifiche presenti
     */
    public @NotNull List<Notifica> getNotifiche(@NotNull TuristaAutenticato turistaAutenticato) {
        return repository.findByUsernameDestinatario(turistaAutenticato.getUsername());
    }


    public @NotNull  Notifica save(@NotNull Notifica build) {
        return repository.save(build);
    }

    /**
     * Elimina le notifiche di un utente
     *
     * @param usernameTurista l'utente che esegue l'operazione
     */
    @Transactional
    public void rimuoviNotificheByUsername(@NotNull String usernameTurista) {
        repository.deleteByUsernameDestinatario(usernameTurista);
    }

    //TODO
    public void creaNotificaCreazionePoi(@NotNull PuntoInteresse puntoInteresse, @NotNull TuristaAutenticato destinatario) {//TODO vedi se riesci
        repository.save(
                new NotificaBuilder()
                        .withTitolo("Info Punti Interesse: " + puntoInteresse.getNome())
                        .withDescrizione("il punto di interesse " + puntoInteresse.getNome() + " è stato creato, da " + puntoInteresse.getCreatore().getUsername() + ", è " + (puntoInteresse.getStato() == Stato.IN_ATTESA ? " In Attesa di Valutazione " : " Approvato "))
                        .withDestinatario(destinatario).build());
    }
}
