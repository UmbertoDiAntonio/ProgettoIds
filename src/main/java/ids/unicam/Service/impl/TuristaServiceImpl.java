package ids.unicam.Service.impl;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class TuristaServiceImpl implements TuristaService {

    private final PoiServiceImpl poiServiceImpl;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl;
    private final NotificaServiceImpl notificaServiceImpl;
    private final CuratoreServiceImpl curatoreServiceImpl;

    @Autowired
    public TuristaServiceImpl(PoiServiceImpl poiServiceImpl, TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl, NotificaServiceImpl notificaServiceImpl, CuratoreServiceImpl curatoreServiceImpl) {
        this.poiServiceImpl = poiServiceImpl;
        this.turistaAutenticatoServiceImpl = turistaAutenticatoServiceImpl;
        this.notificaServiceImpl = notificaServiceImpl;
        this.curatoreServiceImpl = curatoreServiceImpl;
    }

    @Override
    public List<Taggable> findByTag(Tag tag) {
        return poiServiceImpl.findByTag(tag);
    }

    public void creaNotificaReport(PuntoInteresse puntoInteresse, String messaggio) {
        curatoreServiceImpl.findByNomeComune(puntoInteresse.getComune().getNome()).forEach( curatore ->
                notificaServiceImpl.save(new NotificaBuilder().withTitolo("Segnalazione: " + puntoInteresse.getNome())
                        .withDescrizione(messaggio)
                        .withDestinatario(curatore).build()));
    }

    @Override
    public Optional<TuristaAutenticato> accedi(String username, String password) {
        if (turistaAutenticatoServiceImpl.verificaPassword(password, username))
            return turistaAutenticatoServiceImpl.findTuristaByUsername(username);
        return Optional.empty();
    }


}
