package ids.unicam.Service.impl;

import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
import ids.unicam.models.contenuti.notifiche.TipologiaNotifiche;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.stereotype.Service;

@Service
public class NotificaReportServiceImpl {
    private final CuratoreServiceImpl curatoreService;
private final NotificaServiceImpl notificaService;
    public NotificaReportServiceImpl(CuratoreServiceImpl curatoreService, NotificaServiceImpl notificaService) {
        this.curatoreService = curatoreService;
        this.notificaService = notificaService;
    }

    public void creaNotificaReport(PuntoInteresse puntoInteresse, String messaggio) {
        curatoreService.findByNomeComune(puntoInteresse.getComune().getNome()).forEach( curatore ->
                notificaService.save(new NotificaBuilder().withTitolo("Segnalazione: " + puntoInteresse.getNome())
                        .withDescrizione(messaggio)
                        .withDestinatario(curatore).build()));
    }
}
