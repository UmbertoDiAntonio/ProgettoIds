package ids.unicam.controller;

import ids.unicam.models.Service.PoiService;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Stato;
import jakarta.persistence.Transient;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.util.ArrayList;

@Controller
public class CuratoreController {
    private final PoiService poiService;
    @Autowired
    public CuratoreController(PoiService poiService) {
        this.poiService = poiService;
    }




}
