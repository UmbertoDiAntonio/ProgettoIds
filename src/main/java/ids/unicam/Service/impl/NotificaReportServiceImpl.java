package ids.unicam.Service.impl;

import ids.unicam.models.DTO.RichiestaCreazionePoiDTO;
import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
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

    public void creaNotificaReport(RichiestaCreazionePoiDTO poiDTO, String messaggio) {
        curatoreService.findByNomeComune(poiDTO.getCreatore().getComune().getNome()).forEach( curatore ->
                notificaService.save(new NotificaBuilder().withTitolo("Segnalazione: " + poiDTO.getNome())
                        .withDescrizione(messaggio)
                        .withDestinatario(curatore).build()));
    }
}
