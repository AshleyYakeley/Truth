module Data.TypeKT.IOWitnessKT where
{
    import Data.OpenWitness;

    type IOWitnessT = IOWitness;
    type IOWitnessKTT a = IOWitness (a ());
    type IOWitnessKTKTT a = IOWitness (a () ());
    type IOWitnessKKTTT a = IOWitness (a Maybe);
    type IOWitnessKKTTKTT a = IOWitness (a Maybe ());
    IOWitnessKTKTKTT
    IOWitnessKTKTKTKTT
}
