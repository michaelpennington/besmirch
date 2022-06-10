#[cfg(HAS_BACKTRACE)]
use std::backtrace::Backtrace;

use std::fmt::Display;

use thiserror::Error;

const HEADER_CHUNK_ID: &[u8; 4] = b"MThd";
const TRACK_CHUNK_ID: &[u8; 4] = b"MTrk";

#[derive(Debug, Error)]
struct MidiConvertError {
    msg: Option<String>,
    source: MidiConvertErrorTypes,
    #[cfg(HAS_BACKTRACE)]
    backtrace: Backtrace,
}

impl Display for MidiConvertError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = write!(
            f,
            "{}: {}",
            self.source,
            self.msg.as_ref().unwrap_or(&"".into())
        );
        #[cfg(HAS_BACKTRACE)]
        let result = result.and(
            self.backtrace
                .frames()
                .iter()
                .fold(Ok(()), |ok, frame| write!(f, "{:?}", frame).and(ok)),
        );
        result
    }
}

#[derive(Debug, PartialEq, Eq, Error)]
enum MidiConvertErrorTypes {
    #[error("Failed to convert between [u8] and SmpteFps.")]
    Smpte,
    #[error("Invalid conversion from u8 to MidiFormat.")]
    MidiFormat,
    #[error("Invalid chunk id! Must be one of b\"MThd\" or b\"MTrk\"")]
    ChunkId,
    #[error("Header chunk is invalid data")]
    HeaderChunk,
    #[error("Time division is invalid")]
    TimeDivision,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum ChunkId {
    Header,
    Track,
}

impl TryFrom<&[u8]> for ChunkId {
    type Error = MidiConvertError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() != 4 {
            return Err(MidiConvertError {
                msg: Some(format!("chunk id is 4 bytes, given {}", value.len())),
                source: MidiConvertErrorTypes::ChunkId,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        if value == HEADER_CHUNK_ID {
            Ok(Self::Header)
        } else if value == TRACK_CHUNK_ID {
            Ok(Self::Track)
        } else {
            Err(MidiConvertError {
                msg: Some(format!("Chunk id given was {:?}", value)),
                source: MidiConvertErrorTypes::ChunkId,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            })
        }
    }
}

impl From<ChunkId> for &[u8] {
    fn from(chunk_id: ChunkId) -> Self {
        match chunk_id {
            ChunkId::Header => HEADER_CHUNK_ID,
            ChunkId::Track => TRACK_CHUNK_ID,
        }
    }
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum MidiFormat {
    Type0 = 0,
    Type1 = 1,
    Type2 = 2,
}

impl TryFrom<u16> for MidiFormat {
    type Error = MidiConvertError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Type0),
            1 => Ok(Self::Type1),
            2 => Ok(Self::Type2),
            _ => Err(MidiConvertError {
                msg: Some("Midi format must be 0, 1, or 2".into()),
                source: MidiConvertErrorTypes::MidiFormat,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SmpteFps {
    Fmt24 = 24,
    Fmt25 = 25,
    Fmt2997 = 29,
    Fmt30 = 30,
}

impl TryFrom<u8> for SmpteFps {
    type Error = MidiConvertError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            24 => Ok(Self::Fmt24),
            25 => Ok(Self::Fmt25),
            29 => Ok(Self::Fmt2997),
            30 => Ok(Self::Fmt30),
            _ => Err(MidiConvertError {
                msg: Some("Midi format must be one of 24, 25, 29, or 30".into()),
                source: MidiConvertErrorTypes::Smpte,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TimeDivision {
    TicksPerBeat(u16),
    FramesPerSecond(SmpteFps, u8),
}

impl TryFrom<&[u8]> for TimeDivision {
    type Error = MidiConvertError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() != 2 {
            return Err(MidiConvertError {
                msg: Some(format!(
                    "Must be exactly 2 bytes to convert to TimeDivision, got {}",
                    bytes.len()
                )),
                source: MidiConvertErrorTypes::TimeDivision,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        if bytes[0] & 0x80 == 0x80 {
            Ok(TimeDivision::FramesPerSecond(
                (bytes[0] - 0x80).try_into()?,
                bytes[1],
            ))
        } else {
            Ok(TimeDivision::TicksPerBeat(u16::from_be_bytes(
                bytes.try_into().unwrap(),
            )))
        }
    }
}

impl From<TimeDivision> for [u8; 2] {
    fn from(time_div: TimeDivision) -> Self {
        match time_div {
            TimeDivision::TicksPerBeat(ticks) => ticks.to_be_bytes(),
            TimeDivision::FramesPerSecond(fps, tpf) => {
                ((fps as u16 + 0x80) << 8 | tpf as u16).to_be_bytes()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HeaderChunk {
    format_type: MidiFormat,
    num_tracks: u16,
    time_division: TimeDivision,
}

impl Default for HeaderChunk {
    fn default() -> Self {
        Self {
            format_type: MidiFormat::Type0,
            num_tracks: 1,
            time_division: TimeDivision::TicksPerBeat(480),
        }
    }
}

impl From<HeaderChunk> for Vec<u8> {
    fn from(header_chunk: HeaderChunk) -> Self {
        let mut chunk = Vec::with_capacity(14);
        chunk.extend_from_slice(b"MThd"); // 4
        chunk.extend_from_slice(6_u32.to_be_bytes().as_slice()); // 4
        chunk.extend_from_slice(&(header_chunk.format_type as u16).to_be_bytes()); // 2
        chunk.extend_from_slice(&header_chunk.num_tracks.to_be_bytes()); // 2
        let time_division: [u8; 2] = header_chunk.time_division.into();
        chunk.extend_from_slice(&time_division);
        chunk
    }
}

impl TryFrom<&[u8]> for HeaderChunk {
    type Error = MidiConvertError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() != 14 {
            return Err(MidiConvertError {
                msg: Some(format!(
                    "Header wrong size, given a slice {} bytes long",
                    value.len()
                )),
                source: MidiConvertErrorTypes::HeaderChunk,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        let _chunk_id: ChunkId = value[0..4].try_into()?;
        let chunk_len = u32::from_be_bytes(value[4..8].try_into().unwrap());
        if chunk_len != 6 {
            return Err(MidiConvertError {
                msg: Some(format!(
                    "Chunk length must be 6 for a header chunk, got {}",
                    chunk_len
                )),
                source: MidiConvertErrorTypes::HeaderChunk,
                #[cfg(HAS_BACKTRACE)]
                backtrace: Backtrace::capture(),
            });
        }
        let format_type = u16::from_be_bytes(value[8..10].try_into().unwrap()).try_into()?;
        let num_tracks = u16::from_be_bytes(value[10..12].try_into().unwrap());
        let time_division = value[12..14].try_into()?;

        Ok(Self {
            format_type,
            num_tracks,
            time_division,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk_id_try_from() -> Result<(), MidiConvertError> {
        let header_chunk_id: ChunkId = HEADER_CHUNK_ID.as_slice().try_into()?;
        assert_eq!(header_chunk_id, ChunkId::Header);
        let track_chunk_id: ChunkId = TRACK_CHUNK_ID.as_slice().try_into()?;
        assert_eq!(track_chunk_id, ChunkId::Track);
        let trid: Result<ChunkId, MidiConvertError> = b"Hell".as_slice().try_into();
        assert!(trid.is_err());
        assert_eq!(b"MThd", <&[u8]>::from(ChunkId::Header));
        assert_eq!(b"MTrk", <&[u8]>::from(ChunkId::Track));

        Ok(())
    }

    #[test]
    fn test_midi_format_convert() -> Result<(), MidiConvertError> {
        let mid_fmt0: MidiFormat = 0u16.try_into()?;
        let mid_fmt1: MidiFormat = 1u16.try_into()?;
        let mid_fmt2: MidiFormat = 2u16.try_into()?;
        let mid_fmt3: Result<MidiFormat, MidiConvertError> = 3u16.try_into();
        assert_eq!(mid_fmt0, MidiFormat::Type0);
        assert_eq!(mid_fmt1, MidiFormat::Type1);
        assert_eq!(mid_fmt2, MidiFormat::Type2);
        assert!(mid_fmt3.is_err());
        let fmt_two = MidiFormat::Type2 as u16;
        assert_eq!(fmt_two, 2u16);
        Ok(())
    }

    #[test]
    fn test_smpte_fps() -> Result<(), MidiConvertError> {
        let smpte1: SmpteFps = 24u8.try_into()?;
        let smpte2: SmpteFps = 25u8.try_into()?;
        let smpte3: SmpteFps = 29u8.try_into()?;
        let smpte4: SmpteFps = 30u8.try_into()?;
        let smpte_err: Result<SmpteFps, MidiConvertError> = 38u8.try_into();
        assert_eq!(smpte1, SmpteFps::Fmt24);
        assert_eq!(smpte2, SmpteFps::Fmt25);
        assert_eq!(smpte3, SmpteFps::Fmt2997);
        assert_eq!(smpte4, SmpteFps::Fmt30);
        assert!(smpte_err.is_err());
        let fmt_back = SmpteFps::Fmt2997 as u8;
        assert_eq!(fmt_back, 29u8);
        Ok(())
    }

    #[test]
    fn test_time_division() -> Result<(), MidiConvertError> {
        let time_div1: TimeDivision = 0x9840_u16.to_be_bytes().as_slice().try_into()?;
        let time_div2: TimeDivision = 0x9D29_u16.to_be_bytes().as_slice().try_into()?;
        let time_div3: TimeDivision = 0x0074_u16.to_be_bytes().as_slice().try_into()?;
        let time_div4_err: Result<TimeDivision, MidiConvertError> =
            0x8383_u16.to_be_bytes().as_slice().try_into();
        let time_div5_err: Result<TimeDivision, MidiConvertError> =
            0xF840_u16.to_be_bytes().as_slice().try_into();
        assert_eq!(
            time_div1,
            TimeDivision::FramesPerSecond(SmpteFps::Fmt24, 64)
        );
        assert_eq!(
            time_div2,
            TimeDivision::FramesPerSecond(SmpteFps::Fmt2997, 41)
        );
        assert_eq!(time_div3, TimeDivision::TicksPerBeat(0x74));
        assert!(time_div4_err.is_err());
        assert!(time_div5_err.is_err());

        let time_div_bytes: [u8; 2] = TimeDivision::TicksPerBeat(0x73).into();
        let time_div_bytes2: [u8; 2] = TimeDivision::FramesPerSecond(SmpteFps::Fmt25, 69).into();
        assert_eq!(time_div_bytes, 0x0073_u16.to_be_bytes());
        dbg!(time_div_bytes2);
        assert_eq!(time_div_bytes2, 0x9945_u16.to_be_bytes());
        Ok(())
    }

    #[test]
    fn test_header_chunk() -> Result<(), MidiConvertError> {
        let header_chunk: HeaderChunk = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into()?;
        let header_chunk2: HeaderChunk = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x02, 0x00, 0x03, 0x99, 0x45,
        ]
        .as_slice()
        .try_into()?;
        let bad_header: Result<HeaderChunk, MidiConvertError> = [
            0x4F, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into();
        let bad_chunk_len: Result<HeaderChunk, MidiConvertError> = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x30, 0x06, 0x00, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into();
        let bad_midi_version: Result<HeaderChunk, MidiConvertError> = [
            0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x03, 0x00, 0x00, 0x01, 0x01, 0xE0,
        ]
        .as_slice()
        .try_into();
        assert_eq!(header_chunk, HeaderChunk::default());
        assert_eq!(
            header_chunk2,
            HeaderChunk {
                format_type: MidiFormat::Type2,
                num_tracks: 3,
                time_division: TimeDivision::FramesPerSecond(SmpteFps::Fmt25, 69),
            }
        );
        assert!(bad_header.is_err());
        assert!(bad_chunk_len.is_err());
        assert!(bad_midi_version.is_err());
        Ok(())
    }
}
