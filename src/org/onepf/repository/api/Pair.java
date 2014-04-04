package org.onepf.repository.api;

/**
 * Created by ivanoff on 28.03.14.
 */
public class Pair<T, E> {

    public T fst;
    public E snd;

    public Pair(T first, E second) {
        fst = first;
        snd = second;
    }
}
