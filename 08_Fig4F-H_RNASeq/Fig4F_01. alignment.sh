#! /bin/bash
#SBATCH --job-name=hisat2_alignment_v3.genome
#SBATCH --cpus-per-task=16
#SBATCH --mem=32G
#SBATCH --partition=medium
#SBATCH --mail-user=rz40@st-andrews.ac.uk
#SBATCH --mail-type=END,FAIL
#SBATCH --output %x_%j.out
#SBATCH --error %x_%j.err
 
echo "HOSTNAME: $HOSTNAME"
echo $(date)
 
#########################################
## hisat2-alignment                    ##
#########################################

rdir='/home/rezhang/scratch/private/SwRNAseq_20221201/01_DATA/01_Raw_Trimmed_Reads/Trimmed_Together_Reads'

#ls *_1.fastq.gz >1
#ls *_2.fastq.gz >2
#paste 1 2 >config 

indexdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/02.v3.index'
samdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/01_HISAT2_ALIGNMENT'
cat $rdir/config | while read id
do
    arr=(${id})
    fq1=${arr[0]}
    fq2=${arr[1]}
    S=${id%%_*}.sam
hisat2 -p 16 --dta -x $indexdir/02.v3.index -1 \
$rdir/$fq1 -2 \
$rdir/$fq2 -S $samdir/$S
done