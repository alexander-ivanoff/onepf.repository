package org.onepf.repository.api.responsewriter.entity;

import javax.persistence.Column;

/**
 * Created by akarimova on 21.04.14.
 */
public abstract class BaseHashEntity extends BaseEntity {
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
