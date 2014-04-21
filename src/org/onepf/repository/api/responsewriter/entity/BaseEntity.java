package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Created by akarimova on 17.04.14.
 */
@MappedSuperclass
@XmlAccessorType(XmlAccessType.NONE)
public abstract class BaseEntity {
    @Column(name = "currPageHash")
    private int currPageHash;

    @Column(name = "prevPageHash")
    private int prevPageHash;


    public int getCurrPageHash() {
        return currPageHash;
    }

    public void setCurrPageHash(int currPageHash) {
        this.currPageHash = currPageHash;
    }

    public int getPrevPageHash() {
        return prevPageHash;
    }

    public void setPrevPageHash(int prevPageHash) {
        this.prevPageHash = prevPageHash;
    }

}
